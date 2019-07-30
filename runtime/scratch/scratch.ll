declare i8* @minicute_create_node_NInteger(i32)
declare void @minicute_llvm_pointer_debug(...)
declare i32 @printf(i8* noalias nocapture, ...)

%struct.minicute_node = type { i8 }
%struct.minicute_node_NEmpty = type { i8 }
%struct.minicute_node_NInteger = type { i8, i32 }
%struct.minicute_node_NStructure = type { i8, i32, i32, i8** }
%struct.minicute_node_NGlobal = type { i8, i8*, i32 }

@test = private unnamed_addr constant [14 x i8] c"result is %d\0A\00", align 1
@asp = external dso_local global i8**
@nhp = external dso_local global i8*

@minicute__user__defined__node__f = dso_local global %struct.minicute_node_NGlobal { i8 6, i8* bitcast (void ()* @minicute__user__defined__f to i8*), i32 0 }
define dso_local void @minicute__user__defined__f() {
  entry:
    ; PushBasicValue 100
    %0 = alloca i32
    store i32 100, i32* %0
    %1 = load i32, i32* %0

    ; UpdateAsInteger 0
    %2 = call i8* @minicute_create_node_NInteger(i32 %1)
    %3 = load i8**, i8*** @asp
    store i8* %2, i8** %3

    ; Return
    ret void
}

@minicute__user__defined__node__main = dso_local global %struct.minicute_node_NGlobal { i8 6, i8* bitcast (void ()* @minicute__user__defined__main to i8*), i32 0 }
define dso_local void @minicute__user__defined__main() {
  entry:
    ; Example heap allocation
    %0 = call i8* @minicute_create_node_NInteger(i32 50)

    ; Push the allocated node to the stack
    %1 = load i8**, i8*** @asp
    %2 = getelementptr inbounds i8*, i8** %1, i32 1
    store i8* %0, i8** %2
    store i8** %2, i8*** @asp

    call void @minicute__util__print_top_NInteger()
    call void @minicute__user__defined__f()
    call void @minicute__util__print_top_NInteger()

    ret void
}

define private dso_local void @minicute__util__print_top_NInteger() unnamed_addr alwaysinline {
  entry:
    %0 = load i8**, i8*** @asp
    %1 = getelementptr inbounds i8*, i8** %0, i32 0
    %2 = bitcast i8** %1 to %struct.minicute_node_NInteger**
    %3 = load %struct.minicute_node_NInteger*, %struct.minicute_node_NInteger** %2
    %4 = getelementptr inbounds %struct.minicute_node_NInteger, %struct.minicute_node_NInteger* %3, i32 0, i32 1
    %5 = load i32, i32* %4
    %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @test, i32 0, i32 0), i32 %5)

    ret void
}
