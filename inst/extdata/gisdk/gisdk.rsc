// Most basic
Macro "first macro"
    return("Hello World!")
endmacro

// With basic arguments
Macro "add" (int1, int2)
    return(int1 + int2)
endmacro

// With a simple array argument
Macro "first element" (array)
    return(array[1])
endmacro

// Use this to see the structure of an array
Macro "show array" (array)
    ShowArray(array)
endmacro

// Use this to verify option array parsing
Macro "parse opts array" (opts)
    first_name = opts[1][1]
    first_value = opts.(first_name)
    return(
        "The first option name is " + first_name + ". " +
        "The first option value is " + first_value + "."
    )
endmacro