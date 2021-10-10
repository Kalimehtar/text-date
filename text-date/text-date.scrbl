#lang scribble/manual

@(require (for-label racket/gui/base))

@title{Support for date text-field% in racket/gui}
@author[(author+email "Roman Klochkov" "kalimehtar@mail.ru")]

@(defmodule text-date)

@section{Main interface}

Основной интерфейс.

@defclass[date-text-field% text-field% ()]{
 Extends @racket[text-field%] with current date cue and autocomplete date. Supposed to be used to enter
date in dd.mm.yyyy format.

 It allows only digits and "." chars to enter.

 After entering, date can be completed to full. "1" or "01" completes to the first day of the current month,
 "04.05" completes to the 4 May of the current year. Year may be entered with two digits, then the date will be completed to the nearest year
 ending with these digits: in 2021, "55" completes to 2055, "98" completes to 1998.

 Расширяет @racket[text-field%], показывая текущую дату в качестве подсказки. Поддерживает ввод даты в формате дд.мм.гггг.

 При вводе позволяет вводить только цифры и точки.

 После ввода дополняет дату текущим месяцем и годом, если она введена не до конца. "1" или "01" дополняются до первого числа текущего месяца,
 "04.05" дополняется до 4 мая текущего года. Год может быть введён двумя цифрами, тогда он дополняется до ближайшего, оканчивающегося на эти цифры:
 в 2021 году "55" дополняется до 2055, "98" дополняется до 1998.
}