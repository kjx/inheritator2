import "loader" as loader
import "kernan-translator" as translator
import "typechecker" as typechecker
import "type-model" as typemodel

translator.jast := typechecker.singleton
typechecker.ng := typemodel.singleton
loader.objectModel := typemodel.singleton

loader.installIntrinsicModule( typemodel.singleton.intrinsicModuleObject )

loader.loadModulesFromArguments
