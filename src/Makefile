GHC         = ghc
FLAGS       = -Wall -Wno-unused-do-bind -Wno-incomplete-patterns \
			  -Wno-incomplete-uni-patterns
LLVM_DIR    = LLVM/
JVM_DIR     = JVM/
GRAMMAR_DIR = Grammar/
MAIN_LLVM   = Main.hs
MAIN_JVM    = Main.hs
TARGET_LLVM = ../insc_llvm
TARGET_JVM  = ../insc_jvm


all:
	${GHC} ${FLAGS} ${LLVM_DIR}${MAIN_LLVM} -o ${TARGET_LLVM}
	${GHC} ${FLAGS} ${JVM_DIR}${MAIN_JVM} -o ${TARGET_JVM}


clean:
	rm -f ${LLVM_DIR}/*.hi ${LLVM_DIR}/*.o
	rm -f ${JVM_DIR}/*.hi ${JVM_DIR}/*.o
	rm -f ${GRAMMAR_DIR}/*.hi ${GRAMMAR_DIR}/*.o
	rm -f ${TARGET_LLVM}
	rm -f ${TARGET_JVM}
