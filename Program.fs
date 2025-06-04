
open System

/// Функция для безопасного ввода целых чисел с обработкой ошибок
let inputNumbers prompt =
    let rec loop () =
        printf "%s" prompt
        match Int32.TryParse(Console.ReadLine()) with
        | true, num -> num  
        | _ -> 
            printfn "Ошибка! Введите целое число."
            loop ()  
    loop ()

/// Создание списка модулей чисел
let negate x = abs x  
let n = inputNumbers "Размерность списка: "  

// Генерация списка модулей
let result =
    [ for _ in 1..n do  
        let num = inputNumbers "Введите число: "  
        yield negate num  
    ]

printfn "Список модулей: %A" result  

/// Функция для ввода неотрицательных чисел (для факториала)
let inputPositiveNumber prompt =
    let rec loop () =
        printf "%s" prompt
        match Int32.TryParse(Console.ReadLine()) with
        | true, num when num >= 0 -> num  
        | _ -> 
            printfn "Ошибка! Введите целое неотрицательное число."
            loop ()
    loop ()

/// Рекурсивное вычисление факториала
let rec factorial n =
    if n <= 1 then 1  
    else n * factorial (n - 1)  


let num = inputPositiveNumber "Введите число для вычисления факториала: "
printfn "Факториал %d = %d" num (factorial num)

/// Функция ввода списка чисел одной строкой через пробел
let inputList () =
    printfn "Введите числа через пробел:"
    Console.ReadLine().Split(' ')
    |> Array.filter (fun x -> x <> "")  
    |> Array.map (fun x -> 
        match Int32.TryParse(x) with
        | true, num -> num 
        | _ -> 
            printfn $"Не удалось распознать число: '{x}'. Будет заменено на 0."
            0)  
    |> List.ofArray  

/// Основные операции со списками:
let addElement x list = x :: list  // Добавление элемента в начало
let removeElement x list = List.filter ((<>) x) list  // Удаление всех вхождений
let contains x list = List.exists ((=) x) list  // Проверка наличия
let concatLists list1 list2 = list1 @ list2  // Объединение списков
let getElement index list =  // Безопасное получение по индексу
    if index >= 0 && index < List.length list 
    then Some list.[index] 
    else None

/// Основная программа работы со списками
printfn "=== Операции со списками ==="
let list = inputList()  
printfn "Ваш список: %A" list

// 1. Добавление элемента
printf "Введите число для добавления в начало списка: "
let numToAdd = Console.ReadLine() |> int  
let newList = addElement numToAdd list
printfn "Список после добавления: %A" newList

// 2. Удаление элемента
printf "Введите число для удаления: "
let numToRemove = Console.ReadLine() |> int
let filteredList = removeElement numToRemove newList
printfn "Список после удаления: %A" filteredList

// 3. Проверка наличия элемента
printf "Введите число которое хотите найти: "
let numToCheck = Console.ReadLine() |> int
printfn "Число %d %s в списке" numToCheck 
    (if contains numToCheck filteredList then "есть" else "нет")

// 4. Объединение списков
printfn "Введите второй список через пробел для сцепки:"
let list2 = inputList()
let combined = concatLists filteredList list2
printfn "Результат сцепки: %A" combined

// 5. Получение элемента по индексу
printf "Введите индекс элемента: "
let indexToGet = Console.ReadLine() |> int
match getElement indexToGet combined with
| Some x -> printfn "Элемент с индексом %d: %d" indexToGet x
| None -> printfn "Ошибка: недопустимый индекс!"