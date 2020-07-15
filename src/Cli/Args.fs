module Args

open Argu

[<CliPrefix(CliPrefix.Dash)>]
[<NoAppSettings>]
[<RequireSubcommand>]
type CompileArgs =
    | [<MainCommand; ExactlyOnce; Last>] File of file: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | File -> "The path to the Nya source file to compile"

and NyaArgs =
    | Version
    | [<Inherit; AltCommandLine("-v")>] Verbose
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Compile of ParseResults<CompileArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "Prints the version of this copy of the Nya compiler"
            | Verbose -> "Print additional debug output."
            | Compile _ -> "Compile the specified Nya source file"
