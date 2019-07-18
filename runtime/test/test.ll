declare i32 @printf(i8* noalias nocapture, ...)

%Node = type { i32, i32 }

@test = private unnamed_addr constant [14 x i8] c"result is %d\0A\00", align 1
@asp = external global i32*

define void @minicute__user__defined__f() {
  %1 = bitcast i32** getelementptr inbounds (i32*, i32** @asp, i32 1) to i32*
  store i32* %1, i32** @asp

  %2 = bitcast i32* %1 to %Node*
  %3 = getelementptr inbounds %Node, %Node* %2, i32 0, i32 1
  store i32 100, i32* %3

  ret void
}

define void @minicute__user__defined__main() {
  call void @minicute__user__defined__f()
  %1 = load i32*, i32** @asp
  %2 = bitcast i32* %1 to %Node*
  %3 = getelementptr inbounds %Node, %Node* %2, i32 0, i32 1
  %4 = load i32, i32* %3

  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @test, i32 0, i32 0), i32 %4)

  ret void
}
