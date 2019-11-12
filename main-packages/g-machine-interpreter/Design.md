# G-Machine Interpreter Design

## Debugging Commands

- `help`
    Display the command list
- `show`
    - `state`
        Show the current state once
- `display`
    - `state`
        Display the current state everytime it is changed
- `run`
    Execute til the final state
- `next`
    Produce the next state
- `prev`
    Go back to the previous state
- `alloc`
    Allocate a node in the node heap

## Example execution

1. example01
    ```
    $ minicute-g example01.gm
    state<0> {
      code {
        MakeGlobal main;
        Eval;
      }
      address stack [
      ]
      value stack []
      dump {
      }
      node heap<&1> {
        &1: <0> {
          PushBasicValue 0;
          UpdateAsInteger 0;
          Return;
        }
      }
      global {
        main -> &1: <0> { ... }
      }
    }
    ... 5 states ...
    state<6> {
      code {
      }
      address stack [
        &1: 0
      ]
      value stack []
      dump {
      }
      node heap<&1> {
        &1: 0
      }
      global {
        main -> &1: 0
      }
    }
    ```
    
    where `example01.gm` is
    ```
    main<0> {
      PushBasicValue 0;
      UpdateAsInteger 0;
      Return;
    }
    ```
    
2. example02
    ```
    $ minicute-g example02.gm --verbose
    state<0> {
      code {
        MakeGlobal main;
        Eval;
      }
      address stack [
      ]
      value stack []
      dump {
      }
      node heap<&1> {
        &1: <0> {
          MakeInteger 0;
          Update 1;
          Pop 1;
          Unwind;
        }
      }
      global {
        main -> &1: <0> { ... }
      }
    }
    state<1> {
      code {
        Eval;
      }
      address stack [
        &1: <0> { ... }
      ]
      value stack []
      dump {
      }
      node heap<&1> {
        &1: <0> {
          MakeInteger 0;
          Update 1;
          Pop 1;
          Unwind;
        }
      }
      global {
        main -> &1: <0> { ... }
      }
    }
    state<2> {
      code {
        Unwind;
      }
      address stack [
        &1: <0> { ... }
      ]
      value stack []
      dump {
        dump item<0> {
          code {}
          address stack []
          value stack []
        }
      }
      node heap<&1> {
        &1: <0> {
          MakeInteger 0;
          Update 1;
          Pop 1;
          Unwind;
        }
      }
      global {
        main -> &1: <0> { ... }
      }
    }
    state<3> {
      code {
        MakeInteger 0;
        Update 1;
        Pop 1;
        Unwind;
      }
      address stack [
        &1: <0> { ... }
      ]
      value stack []
      dump {
        dump item<0> {
          code {}
          address stack []
          value stack []
        }
      }
      node heap<&1> {
        &1: <0> {
          MakeInteger 0;
          Update 1;
          Pop 1;
          Unwind;
        }
      }
      global {
        main -> &1: <0> { ... }
      }
    }
    state<4> {
      code {
        Update 1;
        Pop 1;
        Unwind;
      }
      address stack [
        &2: 0
        &1: <0> { ... }
      ]
      value stack []
      dump {
        dump item<0> {
          code {}
          address stack []
          value stack []
        }
      }
      node heap<&1> {
        &1: <0> {
          MakeInteger 0;
          Update 1;
          Pop 1;
          Unwind;
        }
        &2: 0
      }
      global {
        main -> &1: <0> { ... }
      }
    }
    state<5> {
      code {
        Pop 1;
        Unwind;
      }
      address stack [
        &2: 0
        &1: ~> &2: 0
      ]
      value stack []
      dump {
        dump item<0> {
          code {}
          address stack []
          value stack []
        }
      }
      node heap<&1> {
        &1: ~> &2: 0
        &2: 0
      }
      global {
        main -> &1: ~> &2: 0
      }
    }
    state<6> {
      code {
        Unwind;
      }
      address stack [
        &1: ~> &2: 0
      ]
      value stack []
      dump {
        dump item<0> {
          code {}
          address stack []
          value stack []
        }
      }
      node heap<&1> {
        &1: ~> &2: 0
        &2: 0
      }
      global {
        main -> &1: ~> &2: 0
      }
    }
    state<7> {
      code {
        Unwind;
      }
      address stack [
        &2: 0
      ]
      value stack []
      dump {
        dump item<0> {
          code {}
          address stack []
          value stack []
        }
      }
      node heap<&1> {
        &1: ~> &2: 0
        &2: 0
      }
      global {
        main -> &1: ~> &2: 0
      }
    }
    state<8> {
      code {
      }
      address stack [
        &2: 0
      ]
      value stack []
      dump {
      }
      node heap<&1> {
        &1: ~> &2: 0
        &2: 0
      }
      global {
        main -> &1: ~> &2: 0
      }
    }
    ```
    
    where `example02.gm` is
    ```
    main<0> {
      MakeInteger 0;
      Update 1;
      Pop 1;
      Unwind;
    }
    ```
    
3. example03
    ```
    $ minicute-g example03.gm --debug
    state {
      code {
        MakeGlobal main;
        Eval;
      }
      address stack [
      ]
      value stack []
      dump {
      }
      node heap<&1> {
        &1: <0> {
          MakeGlobal f;
          Update 1;
          Pop 1;
          Unwind;
        }
        &2: <0> {
          PushBasicValue 1;
          UpdateAsInteger 0;
          Return
        }
      }
      global {
        f    -> &2: <0> { ... }
        main -> &1: <0> { ... }
      }
    }
    > next
    > next
    > next
    > next
    > next
    > next
    > next
    > next
    > next
    > next
    > next
    Execution is terminated.
    state {
      code {
      }
      address stack [
        &2: 1
      ]
      value stack []
      dump {
      }
      node heap<&2> {
        &1: ~> &2: 1
        &2: 1
      }
      global {
        f    -> &2: 1
        main -> &1: ~> &2: 1
      }
    }
    ```
    
    where `example03.gm` is
    ```
    main<0> {
      MakeGlobal f;
      Update 1;
      Pop 1;
      Unwind;
    }
    
    f<0> {
      PushBasicValue 1;
      UpdateAsInteger 0;
      Return;
    }
    ```
