require("../premake-export-compile-commands/export-compile-commands")

PROJECT_GENERATOR_VERSION = 3

newoption({
	trigger = "gmcommon",
	description = "Sets the path to the garrysmod_common (https://github.com/danielga/garrysmod_common) directory",
	value = "../garrysmod_common"
})

include(assert(_OPTIONS.gmcommon or os.getenv("GARRYSMOD_COMMON"),
	"you didn't provide a path to your garrysmod_common (https://github.com/danielga/garrysmod_common) directory"))

CreateWorkspace({ name = "surffix" })
CreateProject({ serverside = true })
IncludeHelpersExtended()
IncludeDetouring()
IncludeScanning()
IncludeSDKCommon()
IncludeSDKTier0()
IncludeSDKTier1()
IncludeSDKMathlib()
