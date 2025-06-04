
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


/// функция ввода числа с проверкой
let inputNumber prompt =
    let rec loop () =
        printf "%s" prompt
        match Int32.TryParse(Console.ReadLine()) with
        | true, num -> num
        | _ -> 
            printfn "Ошибка! Введите целое число."
            loop ()
    loop ()

/// Функция ввода списка с строгой проверкой каждого числа
let inputList () =
    printfn "Введите числа через пробел:"
    let rec processInput (input: string[]) acc index =
        if index >= input.Length then 
            List.rev acc  
        else
            let item = input.[index].Trim()
            if String.IsNullOrEmpty(item) then
                processInput input acc (index + 1)  
            else
                match Int32.TryParse(item) with
                | true, num -> 
                    processInput input (num :: acc) (index + 1)  
                | false, _ -> 
                    printfn $"Ошибка: '{item}' не является числом. Пропускаем."
                    processInput input acc (index + 1) 
    
    let input = Console.ReadLine().Split(' ')
    processInput input [] 0

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
let list = inputList()  // Ввод исходного списка с проверкой
printfn "Ваш список: %A" list

// 1. Добавление элемента (теперь с проверкой)
printf "Введите число для добавления в начало списка: "
let numToAdd = inputNumber ""  // Используем нашу безопасную функцию
let newList = addElement numToAdd list
printfn "Список после добавления: %A" newList

// 2. Удаление элемента (теперь с проверкой)
printf "Введите число для удаления: "
let numToRemove = inputNumber ""
let filteredList = removeElement numToRemove newList
printfn "Список после удаления: %A" filteredList

// 3. Проверка наличия элемента (теперь с проверкой)
printf "Введите число которое хотите найти: "
let numToCheck = inputNumber ""
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
