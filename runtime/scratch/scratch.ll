declare i8* @minicute_create_node_NInteger(i32)
declare void @minicute_update_node_NInteger(i32, i8*)
declare void @minicute_update_node_NIndirect(i8*, i8*)
declare void @minicute_llvm_pointer_debug(...)
declare i32 @printf(i8* noalias nocapture, ...)

%struct.minicute_node = type { i8 }
%struct.minicute_node_NEmpty = type { i8 }
%struct.minicute_node_NInteger = type { i8, i32 }
%struct.minicute_node_NStructure = type { i8, i32, i32, i8** }
%struct.minicute_node_NGlobal = type { i8, i8*, i32 }

@asp = external dso_local global i8**
@nhp = external dso_local global i8*

@minicute__user__defined__node__f = weak_odr dso_local global %struct.minicute_node_NGlobal { i8 6, i8* bitcast (void ()* @minicute__user__defined__f to i8*), i32 0 }
define dso_local void @minicute__user__defined__f() {
  entry:
    ; PushBasicValue 100
    %0 = alloca i32
    store i32 100, i32* %0
    %1 = load i32, i32* %0

    ; UpdateAsInteger 0
    %2 = load i8**, i8*** @asp
    %3 = getelementptr inbounds i8*, i8** %2, i32 0
    %4 = load i8*, i8** %3
    call void @minicute_update_node_NInteger(i32 %1, i8* %4)

    ; Return
    ret void
}

@minicute__user__defined__node__main = weak_odr dso_local global %struct.minicute_node_NGlobal { i8 6, i8* bitcast (void ()* @minicute__user__defined__main to i8*), i32 0 }
define dso_local void @minicute__user__defined__main() {
  entry:
    ; PushGlobal "f"
    %0 = load i8**, i8*** @asp
    %1 = getelementptr inbounds i8*, i8** %0, i32 1
    %2 = bitcast %struct.minicute_node_NGlobal* @minicute__user__defined__node__f to i8*
    store i8* %2, i8** %1
    store i8** %1, i8*** @asp
    ; -- debug
    call void @minicute__util__print_top_node_tag()

    ; Update 1
    %3 = load i8**, i8*** @asp
    %4 = getelementptr inbounds i8*, i8** %3, i32 0
    %5 = load i8*, i8** %4
    %6 = getelementptr inbounds i8*, i8** %3, i32 -1
    %7 = load i8*, i8** %6
    call void @minicute_update_node_NIndirect(i8* %5, i8* %7)

    ; Unwind
    %8 = load i8**, i8*** @asp
    %9 = getelementptr inbounds i8*, i8** %8, i32 0
    %10 = load i8*, i8** %9
    %11 = bitcast i8* %10 to %struct.minicute_node*
    %12 = getelementptr inbounds %struct.minicute_node, %struct.minicute_node* %11, i32 0, i32 0
    %13 = load i8, i8* %12

    switch i8 %13, label %unwind_error_tag
           [ i8 1, label %unwind_NInteger_tag
             i8 2, label %unwind_NConstructor_tag
             i8 3, label %unwind_NStructure_tag
             i8 4, label %unwind_NApplication_tag
             i8 5, label %unwind_NIndirect_tag
             i8 6, label %unwind_NGlobal_tag ]

  unwind_NInteger_tag:
    ret void

  unwind_NConstructor_tag:
    ; Unimplemented
    ret void

  unwind_NStructure_tag:
    ret void

  unwind_NApplication_tag:
    ; Unimplemented
    ret void

  unwind_NIndirect_tag:
    ; Unimplemented
    ret void

  unwind_NGlobal_tag:
    ; TODO: Following code should check the number of arguments
    %14 = bitcast i8* %10 to %struct.minicute_node_NGlobal*
    %15 = getelementptr inbounds %struct.minicute_node_NGlobal, %struct.minicute_node_NGlobal* %14, i32 0, i32 1
    %16 = load i8*, i8** %15
    %17 = bitcast i8* %16 to void ()*
    call void %17()
    ; -- debug
    call void @minicute__util__print_top_node_tag()
    ; -- debug
    call void @minicute__util__print_top_NInteger()

    ret void

  unwind_error_tag:
    ; Unimplemented
    ret void
}

@minicute__util__print_top_node_tag.format = private unnamed_addr constant [20 x i8] c"top node tag is %d\0A\00", align 1
define private dso_local void @minicute__util__print_top_node_tag() unnamed_addr alwaysinline {
  entry:
    %0 = load i8**, i8*** @asp
    %1 = getelementptr inbounds i8*, i8** %0, i32 0
    %2 = load i8*, i8** %1
    %3 = bitcast i8* %2 to %struct.minicute_node*
    %4 = getelementptr inbounds %struct.minicute_node, %struct.minicute_node* %3, i32 0, i32 0
    %5 = load i8, i8* %4
    %6 = sext i8 %5 to i32
    %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @minicute__util__print_top_node_tag.format, i32 0, i32 0), i32 %6)

    ret void
}

@minicute__util__print_top_NInteger.format = private unnamed_addr constant [26 x i8] c"top NInteger value is %d\0A\00", align 1
define private dso_local void @minicute__util__print_top_NInteger() unnamed_addr alwaysinline {
  entry:
    %0 = load i8**, i8*** @asp
    %1 = getelementptr inbounds i8*, i8** %0, i32 0
    %2 = load i8*, i8** %1
    %3 = bitcast i8* %2 to %struct.minicute_node_NInteger*
    %4 = getelementptr inbounds %struct.minicute_node_NInteger, %struct.minicute_node_NInteger* %3, i32 0, i32 1
    %5 = load i32, i32* %4
    %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([26 x i8], [26 x i8]* @minicute__util__print_top_NInteger.format, i32 0, i32 0), i32 %5)

    ret void
}
