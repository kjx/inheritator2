import "loader" as loader
import "kernan-translator" as translator
import "evaluator" as eval
import "object-model" as runtime

translator.jast := eval.singleton
eval.ng := runtime.singleton
loader.objectModel := runtime.singleton

loader.installIntrinsicModule( runtime.singleton.intrinsicModuleObject )

loader.loadModulesFromArguments

