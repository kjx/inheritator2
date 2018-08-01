import "platform/KernanCompiler" as kc
import "loader" as loader
import "kernan-translator" as translator
import "evaluator" as eval
translator.jast := eval.singleton

loader.loadModulesFromArguments

