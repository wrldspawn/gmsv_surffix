if os.getenv("BUILD_32BIT") == 1 then
	PROJECT_GENERATOR_VERSION = 2
else
	PROJECT_GENERATOR_VERSION = 3
end

newoption({
	trigger = "gmcommon",
	description = "Sets the path to the garrysmod_common (https://github.com/danielga/garrysmod_common) directory",
	value = "../garrysmod_common"
})

local gmcommon = assert(_OPTIONS.gmcommon or os.getenv("GARRYSMOD_COMMON"),
	"you didn't provide a path to your garrysmod_common (https://github.com/danielga/garrysmod_common) directory")
require(gmcommon .. "/premake/premake-export-compile-commands/export-compile-commands")
include(gmcommon)

CreateWorkspace({ name = "surffix" })
CreateProject({ serverside = true })
IncludeHelpersExtended()
IncludeDetouring()
IncludeScanning()
IncludeSDKCommon()
IncludeSDKTier0()
IncludeSDKTier1()
IncludeSDKMathlib()
