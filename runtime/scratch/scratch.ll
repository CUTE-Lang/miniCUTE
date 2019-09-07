declare i8* @minicute_create_node_NInteger(i32)
declare void @minicute_update_node_NInteger(i32, i8*)
declare void @minicute_update_node_NIndirect(i8*, i8*)
declare i32 @printf(i8* noalias nocapture, ...)

%struct.minicute_node = type { i8 }
%struct.minicute_node_NEmpty = type { i8 }
%struct.minicute_node_NInteger = type { i8, i32 }
%struct.minicute_node_NStructure = type { i8, i32, i32, i8** }
%struct.minicute_node_NIndirect = type { i8, i8* }
%struct.minicute_node_NGlobal = type { i8, i8*, i32 }

@abp = external dso_local unnamed_addr global i8**
@asp = external dso_local unnamed_addr global i8**
@nhp = external dso_local unnamed_addr global i8*

; f<0> {
;   PushBasicValue 100;
;   UpdateAsInteger 0;
;   Return;
; }
@minicute__user__defined__node__f = weak_odr dso_local unnamed_addr global %struct.minicute_node_NGlobal { i8 6, i8* bitcast (void ()* @minicute__user__defined__f to i8*), i32 0 }
define dso_local void @minicute__user__defined__f() #1 {
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
    %5 = load i8**, i8*** @abp
    %6 = getelementptr inbounds i8*, i8** %5, i32 1
    store i8** %6, i8*** @asp
    ret void
}

; main<0> {
;   MakeGlobal f;
;   Eval;
;   Update 1;
;   Pop 1;
;   Unwind;
; }
@minicute__user__defined__node__main = weak_odr dso_local unnamed_addr global %struct.minicute_node_NGlobal { i8 6, i8* bitcast (void ()* @minicute__user__defined__main to i8*), i32 0 }
define dso_local void @minicute__user__defined__main() #1 {
  entry:
    ; -- debug
    call void @minicute__util__print_stack_size()
    ; MakeGlobal f
    %0 = load i8**, i8*** @asp
    %1 = getelementptr inbounds i8*, i8** %0, i32 1
    %2 = bitcast %struct.minicute_node_NGlobal* @minicute__user__defined__node__f to i8*
    store i8* %2, i8** %1
    store i8** %1, i8*** @asp
    ; -- debug
    call void @minicute__util__print_stack_size()
    ; -- debug
    call void @minicute__util__print_top_node_tag()

    ; Eval
    %3 = load i8**, i8*** @abp
    %4 = load i8**, i8*** @asp
    %5 = getelementptr inbounds i8*, i8** %4, i32 -1
    store i8** %5, i8*** @abp
    call void @minicute__util__unwind()
    store i8** %3, i8*** @abp
    ; -- debug
    call void @minicute__util__print_stack_size()
    ; -- debug
    call void @minicute__util__print_top_node_tag()

    ; Update 1
    %6 = load i8**, i8*** @asp
    %7 = getelementptr inbounds i8*, i8** %6, i32 0
    %8 = load i8*, i8** %7
    %9 = getelementptr inbounds i8*, i8** %6, i32 -1
    %10 = load i8*, i8** %9
    call void @minicute_update_node_NIndirect(i8* %8, i8* %10)
    ; -- debug
    call void @minicute__util__print_stack_size()
    ; -- debug
    call void @minicute__util__print_top_node_tag()

    ; Pop 1
    %11 = load i8**, i8*** @asp
    %12 = getelementptr inbounds i8*, i8** %11, i32 -1
    store i8** %12, i8*** @asp
    ; -- debug
    call void @minicute__util__print_stack_size()
    ; -- debug
    call void @minicute__util__print_top_node_tag()

    ; Unwind
    call void @minicute__util__unwind()
    ; -- debug
    call void @minicute__util__print_stack_size()
    ; -- debug
    call void @minicute__util__print_top_node_tag()

    ret void
}

; TODO: extract this as a lib function
define private dso_local void @minicute__util__unwind() {
  entry:
    br label %unwind

  unwind:
    ; -- debug
    call void @minicute__util__print_stack_size()
    ; -- debug
    call void @minicute__util__print_top_node_tag()
    ; Unwind
    %unwind.0 = load i8**, i8*** @asp
    %unwind.1 = load i8*, i8** %unwind.0
    %unwind.2 = bitcast i8* %unwind.1 to %struct.minicute_node*
    %unwind.3 = getelementptr inbounds %struct.minicute_node, %struct.minicute_node* %unwind.2, i32 0, i32 0
    %unwind.4 = load i8, i8* %unwind.3

    switch i8 %unwind.4, label %unwind_error_tag
           [ i8 1, label %unwind_NInteger_tag
             i8 2, label %unwind_NConstructor_tag
             i8 3, label %unwind_NStructure_tag
             i8 4, label %unwind_NApplication_tag
             i8 5, label %unwind_NIndirect_tag
             i8 6, label %unwind_NGlobal_tag ]

  unwind_NInteger_tag:
    %unwind.NInteger.0 = load i8**, i8*** @asp
    %unwind.NInteger.1 = load i8*, i8** %unwind.NInteger.0
    %unwind.NInteger.2 = load i8**, i8*** @abp
    %unwind.NInteger.3 = getelementptr inbounds i8*, i8** %unwind.NInteger.2, i32 1
    store i8* %unwind.NInteger.1, i8** %unwind.NInteger.3
    store i8** %unwind.NInteger.3, i8*** @asp
    ret void

  unwind_NConstructor_tag:
    ; Unimplemented
    ret void

  unwind_NStructure_tag:
    %unwind.NStructure.0 = load i8**, i8*** @asp
    %unwind.NStructure.1 = load i8*, i8** %unwind.NStructure.0
    %unwind.NStructure.2 = load i8**, i8*** @abp
    %unwind.NStructure.3 = getelementptr inbounds i8*, i8** %unwind.NStructure.2, i32 1
    store i8* %unwind.NStructure.1, i8** %unwind.NStructure.3
    store i8** %unwind.NStructure.3, i8*** @asp
    ret void

  unwind_NApplication_tag:
    ; Unimplemented
    ret void

  unwind_NIndirect_tag:
    %unwind.NIndirect.0 = bitcast i8* %unwind.1 to %struct.minicute_node_NIndirect*
    %unwind.NIndirect.1 = getelementptr inbounds %struct.minicute_node_NIndirect, %struct.minicute_node_NIndirect* %unwind.NIndirect.0, i32 0, i32 1
    %unwind.NIndirect.2 = load i8*, i8** %unwind.NIndirect.1
    store i8* %unwind.NIndirect.2, i8** %unwind.0

    br label %unwind

  unwind_NGlobal_tag:
    ; TODO: Following code should rearrange the stack
    ; TODO: Following code should check the number of arguments
    %unwind.NGlobal.0 = bitcast i8* %unwind.1 to %struct.minicute_node_NGlobal*
    %unwind.NGlobal.1 = getelementptr inbounds %struct.minicute_node_NGlobal, %struct.minicute_node_NGlobal* %unwind.NGlobal.0, i32 0, i32 1
    %unwind.NGlobal.2 = load i8*, i8** %unwind.NGlobal.1
    %unwind.NGlobal.3 = bitcast i8* %unwind.NGlobal.2 to void ()*
    call void %unwind.NGlobal.3()

    ret void

  unwind_error_tag:
    ; Unimplemented
    ret void
}

@minicute__util__print_stack_size.format = private unnamed_addr constant [26 x i8] c"current stack size is %d\0A\00", align 1
define private dso_local void @minicute__util__print_stack_size() unnamed_addr #0 {
  entry:
    %0 = load i8**, i8*** @abp
    %1 = ptrtoint i8** %0 to i32
    %2 = load i8**, i8*** @asp
    %3 = ptrtoint i8** %2 to i32
    %4 = sub i32 %3, %1
    %5 = sdiv exact i32 %4, 8
    %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([26 x i8], [26 x i8]* @minicute__util__print_stack_size.format, i32 0, i32 0), i32 %5)

    ret void
}

@minicute__util__print_top_node_tag.format = private unnamed_addr constant [20 x i8] c"top node tag is %d\0A\00", align 1
define private dso_local void @minicute__util__print_top_node_tag() unnamed_addr #0 {
  entry:
    %0 = load i8**, i8*** @asp
    %1 = getelementptr inbounds i8*, i8** %0, i32 0
    %2 = load i8*, i8** %1
    %3 = bitcast i8* %2 to %struct.minicute_node*
    %4 = getelementptr inbounds %struct.minicute_node, %struct.minicute_node* %3, i32 0, i32 0
    %5 = load i8, i8* %4
    ; 'sext' is required since -1 is a valid tag
    %6 = sext i8 %5 to i32
    %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @minicute__util__print_top_node_tag.format, i32 0, i32 0), i32 %6)

    ret void
}

@minicute__util__print_top_NInteger.format = private unnamed_addr constant [26 x i8] c"top NInteger value is %d\0A\00", align 1
define private dso_local void @minicute__util__print_top_NInteger() unnamed_addr #0 {
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

attributes #0 = { alwaysinline nounwind }
attributes #1 = { nounwind }
