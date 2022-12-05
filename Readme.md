#base_calc

Обязательный файл настроек(находится в папке релиза): /priv/conn_conf

Формат файла:

<li> wdir путь до папки, которая будет использоваться приложением в работе, $dir
<li> conns количество активных соединений(статичные процессы, обрабатывающие входящие соединения)
<li> port номер порта для подключения
<li> maxmem максимальное количество используемой виртуальной машиной памяти(приблизительно)

Файл, исполняемый при обращении по адресу IPaddr:Port/admin : $dir/doit.txt
  
Формат файла:
  
произвольное количество строк, начинающихся с: d f k l m s и обязательно имеющих параметр, даже если он не указан. Изьвинити.
  
<li> d Tid удалить процесс задачи с идентификатором Tid
<li> f path-name обработать файл задачи
<li> k убить все процессы задач
<li> l вывести в файл $dir/aout.txt данные по работающим процессам задач
<li> m newmaxmem задать динамически новый предел потребления памяти иртуальной машиной
<li> s остановить приложение

Программа обеспечивает обработку чисел произвольной длины и точности по заданному алгоритму. Доступны 4 операции(+ - * /), условие и постусловие, позволяющие выбирать выполняемый блок.

Формат файла задачи:

>R=precision A=(list_of_vars)
>[expression]

expression:= Var=arith_expr | ? | ?Var1 cond Var2 | ?? | ??Var1 cond Var2

cond:= > | < | = | >= | =< | <>

<li> precision целое число >=0
<li> list_of_vars список переменных для вывода результата, они должны быть задействованы в задаче
  
Пустая строка завершает файл задачи, т. е. после неё ничего не обрабатывается.

Унарный минус явно применяется только один раз: --6 вызовет ошибку лишнего оператора.

arith_expr типичное арифметическое выражение со скобками и унарным минусом

Есть пред- и постусловия. Значения в условиях задаются только переменными, т. е. сперва инициализируем переменную числом или другой переменной/арифметическим выражением, потом применяем в условии.
  
Предусловие - типичный WHILE:

> ?Var1 cond Var2
  <br>
> expression
  <br>
> ?

Постусловие DO..WHILE:

> ??
  <br>
> expression
  <br>
> ??Var1 cond Var2

Со страницы в баузере доступны три операции для пользователей и одна для локального администратора:
<li> - загрузить фал задачи и получить Tid;
<li> - узнать состояние выполнения/результат;
<li> - получить справку;
<li> ---- выполнить файл $dir/doit.txt на локальном компьютере

