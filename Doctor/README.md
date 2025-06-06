## Введение

«Доктор» (или ELIZA) -- это название программы, созданной Джозефом Вейценбаумом. Эта программа имитирует (или пародирует) психоаналитика, ведущего диалог с пациентом. Программа принимает реплики пациента (в виде списков символов) и генерирует ответные реплики (также в виде списков). Для простоты сокращена пунктуация в записи реплик. Исходный код ELIZA на языках LISP-семейства можно найти в Сети: [html](https://github.com/apg/chatter/blob/master/eliza.scm), [html](http://jeffshrager.org/llisp/26.html).

В рамках выполнения практического задания Вы создадите собственную версию «Доктора», которая основана на некоторых (но не всех!) идеях, лежащих в основе исходной программы.

Упражнения по «Доктору» делятся на 4 блока:
- [1-й блок](./docs/first_block.md) -- упражнения с 1 по 4.
- [2-й блок](./docs/second_block.md) -- упражнения 5 и 6.
- [3-й блок](./docs/third_block.md) -- упражнение 7.
- [4-й блок](./docs/optional_block.md) -- упражнение по желанию.

Рекомендуется сдавать упражнения блоками. Версии программы 1-3 блоков следует составлять на подмножестве языка scheme/base (начинайте свой код с директивы #lang scheme/base). В них использование мутаторов (присваиваний и т. п.), мутируемых структур данных Racket, запрещено. Можно использовать вектора (но нельзя их мутировать). Составленная в ходе выполнения упражнений, программа должна быть показана лектору в ходе офлайновой сдачи или сдачи через Zoom. При переходе от начальных упражнений к последующим код следует дописывать так, чтобы функциональность программы расширялась (то, что было раньше, не портить, оставлять в виде комментария или под другим именем). Не следует сдавать несколько разных версий программы, каждая из которых решает только одно из упражнений блока.

Сдаваемый код должен быть оформлен должным образом: быть читаемым, сопровождаться содержательными и актуальными комментариями на русском языке. Комментарии, присутствующие в заготовке кода следует переписывать, если их прежние формулировки утрачивают актуальность.