namespace Expecto
open Expecto

module Expect =
    let expect expected actual = Expect.equal actual expected ""
