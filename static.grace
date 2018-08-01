import "platform/KernanCompiler" as kc
import "loader" as loader
import "kernan-translator" as translator
import "typechecker" as typechecker
translator.jast := typechecker.singleton

loader.loadModulesFromArguments
