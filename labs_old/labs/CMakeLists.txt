set(LAB_LIST lab2 lab3)

foreach(LAB ${LAB_LIST})
  add_subdirectory(${LAB})
endforeach(LAB ${LAB_LIST})

set(LAB_LIST
    ${LAB_LIST}
    PARENT_SCOPE)
