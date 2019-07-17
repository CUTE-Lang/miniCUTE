declare i32 @printf(i8* noalias nocapture, ...)
declare i8* @malloc(i32)

%Node = type { i32, i32 }

@test = private unnamed_addr constant [14 x i8] c"result is %d\0A\00", align 1
@sp = private unnamed_addr global i32* null, align 1

define void @f() {
  %1 = bitcast i32** getelementptr inbounds (i32*, i32** @sp, i32 1) to i32*
  store i32* %1, i32** @sp

  %2 = bitcast i32* %1 to %Node*
  %3 = getelementptr inbounds %Node, %Node* %2, i32 0, i32 1
  store i32 100, i32* %3

  ret void
}

define i32 @main() {
  %1 = call i8* @malloc(i32 1024)
  %2 = bitcast i8* %1 to i32*
  store i32* %2, i32** @sp

  call void @f()
  %3 = load i32*, i32** @sp
  %4 = bitcast i32* %3 to %Node*
  %5 = getelementptr inbounds %Node, %Node* %4, i32 0, i32 1
  %6 = load i32, i32* %5

  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @test, i32 0, i32 0), i32 %6)
  ret i32 0
}
