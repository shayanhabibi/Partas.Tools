module Partas.Tools.Cli.Binding

open System
open Microsoft.FSharp.Reflection

/// Sets the casing for the Union cases
type Casing =
    /// <summary>
    /// Changes to kebab-case
    /// <code>
    /// KebabCase -> kebab-case
    /// </code>
    /// </summary>
    | KebabCase = 0
    /// <summary>
    /// No changes made to casing.
    /// </summary>
    | None = 1

/// Defines ancillary modifiers to output
type BindingOptions =
    /// <summary>
    /// Default (no modifications).
    /// </summary>
    | Default = 0
    /// <summary>
    /// Modifies the fields to be wrapped as string literals.
    /// <code lang="fsharp">
    /// [&lt;CliBinding(BindingOptions = BindingOptions.StringLiteralFields)>]
    /// type Test =
    ///     | Format of string
    /// 
    /// Format "test" |> CliBinding.toString // -> --format="test"
    /// </code>
    /// </summary>
    | StringLiteralFields = 1

/// <summary>
/// Allows modified stringification of DUs with <c>CliBinding.toString</c>. 
/// </summary>
/// <remarks>
/// <para>This is useful when creating DUs which are representative of CLI argument options/flags.</para>
/// <para>It abstracts the stringification of these unions to make binding CLI options for
/// tools like Git faster and less time consuming.</para>
/// </remarks>
/// <param name="Casing">Effects the casing of the output; defaults to <c>Casing.KebabCase</c></param>
/// <param name="Prefix">The prefix applies to the CLI arg; defaults to <c>"--"</c></param>
/// <param name="Equals">The string used between a DU case name and its field(s); defaults to <c>"="</c></param>
/// <param name="BindingOptions">Ancillary modifiers; defaults to <c>BindingOptions.Default</c> (ie no modifications).</param>
type CliBindingAttribute() =
    inherit Attribute()
    [<DefaultValue>]
    val mutable Casing: Casing
    [<DefaultValue>]
    val mutable Prefix: string
    [<DefaultValue>]
    val mutable Equals: string
    [<DefaultValue>]
    val mutable BindingOptions: BindingOptions

type private CliBindingInfo = {
    Name: string
    Casing: Casing
    Prefix: string
    EqualsChar: string
    FieldValues: obj list
    Options: BindingOptions
}

[<AutoOpen>]
module private Reflection =
    open System.Reflection
    open FSharp.Reflection
    type AttributeSearchResult =
        {
            Local: CustomAttributeData seq
            DeclaringType: CustomAttributeData seq
        }
    
    let private getDeclaringAttributes (info: UnionCaseInfo) =
        info.DeclaringType.GetCustomAttributesData()
    let private getCaseAttributes (info: UnionCaseInfo) =
        info.GetCustomAttributesData()
    
    let private findAttribute typ: CustomAttributeData seq -> _ =
        Seq.tryFind(_.AttributeType >> (=) typ)
    
    let inline private getMemberValue memberName: CustomAttributeData -> _ =
        _.NamedArguments
        >> Seq.tryFind (_.MemberName >> (=) memberName)
        >> Option.map (_.TypedValue.Value >> unbox)
    
    let private getConstructorValue index: CustomAttributeData -> _ =
        _.ConstructorArguments
        >> function
            | values when values |> Seq.length > index ->
                values |> Seq.item index
                |> _.Value |> tryUnbox
            | _ -> None
    
    let private getAttributes typs: CustomAttributeData seq -> _ =
        Seq.filter (
            fun attr ->
                typs
                |> Seq.contains attr.AttributeType
            )
    
    let searchCaseFor (typs: Type seq) (info: UnionCaseInfo) =
        let localAttrs = getCaseAttributes info
        let typeAttrs = getDeclaringAttributes info
        {
            Local = localAttrs |> getAttributes typs
            DeclaringType = typeAttrs |> getAttributes typs
        }
        
    let getMemberValueOrElse (typ: Type) memberName (orElse: 'T): AttributeSearchResult -> _ =
        function
        | { DeclaringType = decls; Local = locals } ->
            locals
            |> findAttribute typ
            |> Option.bind (getMemberValue memberName)
            |> Option.orElse (
                decls
                |> findAttribute typ
                |> Option.bind (getMemberValue memberName)
            )
            |> Option.defaultValue orElse
    let getPositionValueOrElse (typ: Type) index (orElse: 'T): AttributeSearchResult -> _ =
        function
        | { DeclaringType = decls; Local = locals } ->
            locals
            |> findAttribute typ
            |> Option.bind (getConstructorValue index)
            |> Option.orElse (
                decls
                |> findAttribute typ
                |> Option.bind (getConstructorValue index)
                )
            |> Option.defaultValue orElse

module private CliBindingInfo =
    let getInfo case: CliBindingInfo =
        let info,fields = FSharpValue.GetUnionFields(case, case.GetType())
        let attributeSearchResult =
            searchCaseFor [
                typeof<CliBindingAttribute>
                typeof<CompiledNameAttribute>
            ] info
        {
            Name =
                getPositionValueOrElse
                    typeof<CompiledNameAttribute>
                    0
                    info.Name
                    attributeSearchResult
            Casing =
                getMemberValueOrElse
                    typeof<CliBindingAttribute>
                    "Casing"
                    Casing.KebabCase
                    attributeSearchResult
            Prefix =
                getMemberValueOrElse
                    typeof<CliBindingAttribute>
                    "Prefix"
                    "--"
                    attributeSearchResult
            EqualsChar =
                getMemberValueOrElse
                    typeof<CliBindingAttribute>
                    "Equals"
                    "="
                    attributeSearchResult
            FieldValues = fields |> Array.toList
            Options =
                getMemberValueOrElse
                    typeof<CliBindingAttribute>
                    "BindingOptions"
                    BindingOptions.Default
                    attributeSearchResult
        }

module Casing =
    let toString (casing: Casing) (input: string) =
        match casing with
        | Casing.None ->
            input
        | Casing.KebabCase
        | _ ->
            input[1..]
            |> Seq.fold (
                fun state value ->
                    let isUpper = value |> Char.IsUpper
                    match isUpper with
                    | true ->
                        state + "-" + (value |> Char.ToLower |> string)
                    | false ->
                        state + (value |> string)
                ) ""
            |> (+) (input[0] |> Char.ToLower |> string)

module CliBinding =
    open CliBindingInfo
    let toString (value: obj) =
        let info = getInfo value
        let equals = info.EqualsChar
        let casingConverter = Casing.toString info.Casing
        let name = info.Prefix + (info.Name |> casingConverter)
        let fieldModifier = info.Options |> function
            | BindingOptions.StringLiteralFields ->
                fun value -> $"\"{value}\""
            | _ -> _.ToString()
        match info.FieldValues with
        | [] -> ""
        | [ fieldVal ] ->
            $"{equals}{fieldVal |> fieldModifier}"
        | values ->
            values
            |> List.fold (fun state item ->
                state + $"{equals}{item |> fieldModifier}") ""
        |> (+) name

open BlackFox.CommandLine

module CmdLine =
    let appendDu (union: obj) =  CmdLine.append (union |> CliBinding.toString)
