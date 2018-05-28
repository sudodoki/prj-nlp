### General
Фінальна система складається з 4-ох правил, які мають вагові коефіцієнти. 
З точки зору метрики intersection-over-union (для кожного true приклада знаходиться
predicted приклад, для яких string similarity не менше x%) точність системи
складає 10.43% при оптимально налаштованих параметрах.
 
 ### Error analysis
Знайдено "christ" -> в golden маємо "christ among the doctors"
"madonna with child" -> "haller madonna" (але це ідентичні картини)
"the martyrdom of the ten thousand" -> "martyrdom of the ten thousand"
"frederick the wise" -> "portrait of frederick iii of saxony"
"a small boy peeling a fruit" -> "boy peeling a fruit"
