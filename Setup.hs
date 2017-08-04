import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.PackageDescription

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "env"
        ["make", "--directory=MediaProcessor"]
    return emptyHookedBuildInfo

main = defaultMainWithHooks simpleUserHooks { preConf = makeExtLib }
