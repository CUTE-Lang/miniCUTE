declare void @minicute_llvm_pointer_debug(...)
declare i32 @printf(i8* noalias nocapture, ...)

%node.NInteger = type { i32, i32 }

@test = private unnamed_addr constant [14 x i8] c"result is %d\0A\00", align 1
@asp = external dso_local global i8**
@nhp = external dso_local global i8*

define dso_local void @minicute__user__defined__f() {
  entry:
    ; PushBasicValue 100
    %0 = alloca i32
    store i32 100, i32* %0
    %1 = load i32, i32* %0

    ; UpdateAsInteger 0
    %2 = load i8*, i8** @nhp
    %3 = bitcast i8* %2 to %node.NInteger*
    %4 = getelementptr inbounds %node.NInteger, %node.NInteger* %3, i32 1
    %5 = bitcast %node.NInteger* %4 to i8*
    store i8* %5,i8** @nhp
    %6 = getelementptr inbounds %node.NInteger, %node.NInteger* %3, i32 0, i32 0
    store i32 1, i32* %6
    %7 = getelementptr inbounds %node.NInteger, %node.NInteger* %3, i32 0, i32 1
    store i32 %1, i32* %7

    %8 = load i8**, i8*** @asp
    store i8* %2, i8** %8

    ; Return
    ret void
}

define dso_local void @minicute__user__defined__main() {
  entry:
    ; Example heap allocation
    %0 = load i8*, i8** @nhp
    %1 = bitcast i8* %0 to %node.NInteger*
    %2 = getelementptr inbounds %node.NInteger, %node.NInteger* %1, i32 1
    %3 = bitcast %node.NInteger* %2 to i8*
    store i8* %3,i8** @nhp
    %4 = getelementptr inbounds %node.NInteger, %node.NInteger* %1, i32 0, i32 0
    store i32 1, i32* %4
    %5 = getelementptr inbounds %node.NInteger, %node.NInteger* %1, i32 0, i32 1
    store i32 50, i32* %5

    ; Push the allocated node to the stack
    %6 = load i8**, i8*** @asp
    %7 = getelementptr inbounds i8*, i8** %6, i32 1
    store i8* %0, i8** %7
    store i8** %7, i8*** @asp

    call void @minicute__util__print_top_NInteger()
    call void @minicute__user__defined__f()
    call void @minicute__util__print_top_NInteger()

    ret void
}

define dso_local void @minicute__util__print_top_NInteger() {
  entry:
    %0 = load i8**, i8*** @asp
    %1 = getelementptr inbounds i8*, i8** %0, i32 0
    %2 = bitcast i8** %1 to %node.NInteger**
    %3 = load %node.NInteger*, %node.NInteger** %2
    %4 = getelementptr inbounds %node.NInteger, %node.NInteger* %3, i32 0, i32 1
    %5 = load i32, i32* %4
    %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @test, i32 0, i32 0), i32 %5)

    ret void
}
