declare i32 @printf(i8* noalias nocapture, ...)

%node.NInteger = type { i32, i32 }

@test = private unnamed_addr constant [14 x i8] c"result is %d\0A\00", align 1
@asp = external dso_local global i8*

define dso_local void @minicute__user__defined__f() {
  entry:
    ; PushBasicValue 100
    %0 = alloca i32
    store i32 100, i32* %0
    %1 = load i32, i32* %0

    ; UpdateAsInteger 0
    %2 = bitcast i8** getelementptr inbounds (i8*, i8** @asp, i32 0) to %node.NInteger*
    %3 = getelementptr inbounds %node.NInteger, %node.NInteger* %2, i32 0, i32 0
    store i32 1, i32* %3
    %4 = getelementptr inbounds %node.NInteger, %node.NInteger* %2, i32 0, i32 1
    store i32 %1, i32* %4

    ; Return
    ret void
}

define dso_local void @minicute__user__defined__main() {
  call void @minicute__user__defined__f()
  %1 = getelementptr inbounds %node.NInteger, %node.NInteger* bitcast (i8** getelementptr inbounds (i8*, i8** @asp, i32 0) to %node.NInteger*), i32 0, i32 1
  %2 = load i32, i32* %1

  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @test, i32 0, i32 0), i32 %2)

  ret void
}
