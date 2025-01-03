<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Сурмачевський Владислав Володимирович</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці,
геш-таблиці у асоціативні списки,
асоціативні списки у геш-таблиці.

6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
## Варіант 6
База даних - Проєкти із застосуванням ШІ
Тип записів - Асоціативний список
## Лістинг реалізації завдання
    (defun create-ai-models-csv ()
      "Створює CSV файл з даними про моделі ШІ"
      (let ((filepath "C:/Progrm/portacle/projects/ai-models.csv")
            (headers '("name" "type" "release_date" "company" "parameters"))
            (data '(("GPT-4" "LLM" "14.03.2023" "OpenAI" "1.76T")
                    ("Claude 2" "LLM" "11.07.2023" "Anthropic" "unknown")
                    ("PaLM 2" "LLM" "10.05.2023" "Google" "340B")
                    ("DALL-E 3" "Image" "20.09.2023" "OpenAI" "unknown")
                    ("Stable Diffusion 3" "Image" "16.10.2023" "Stability AI" "unknown"))))
        (write-csv filepath (cons headers data))))
    
    (defun split-string (string delimiter)
      "Розбиває STRING на підрядки за DELIMITER."
      (loop with start = 0
            for pos = (position delimiter string :start start)
            collect (subseq string start (or pos (length string)))
            do (setf start (1+ (or pos (length string))))
            while pos))
    
    (defun read-csv (filepath)
      "Читає CSV-файл та розбиває його на рядки."
      (with-open-file (stream filepath)
        (loop for line = (read-line stream nil)
              while line
              collect (split-string (string-trim '(#\Space #\Return #\Newline) line) #\;))))
    
    (defun write-csv (filepath data)
      "Записує дані у CSV-файл."
      (with-open-file (stream filepath :direction :output :if-exists :supersede)
        (loop for row in data do
              (format stream "~{~a~^;~}~%" row))))
    
    (defun select (filepath &key filter)
      "Створює лямбда-вираз для вибірки записів з CSV-файлу.
    FILTER - функція, яка приймає запис і повертає T, якщо запис потрібно включити."
      (let ((data (read-csv filepath)))
        (let ((headers (first data))
              (rows (rest data)))
          (lambda (&key (where nil))
            (let ((filtered-rows
                    (if filter
                        (remove-if-not filter rows)
                        rows)))
              (if where
                  (remove-if-not
                   (lambda (row)
                     (every (lambda (key-val)
                              (let* ((key (first key-val))
                                     (value (second key-val))
                                     (index (position key headers :test #'string=)))
                                (and index (string= (nth index row) value))))
                            where))
                   filtered-rows)
                  filtered-rows))))))
    
    (defun convert-to-hash-tables (data headers)
      "Конвертує дані з таблиці в хеш-таблиці з використанням заголовків."
      (mapcar (lambda (row)
                (let ((hash (make-hash-table :test #'equal)))
                  (loop for header in headers
                        for value in row do
                          (setf (gethash header hash) value))
                  hash))
              data))
    
    (defun pretty-print-hash (hash-table)
      "Виводить хеш-таблицю у покращеному форматі."
      (format t "~%")  ; Порожній рядок перед таблицею
      (maphash 
       (lambda (key value)
         (format t "    ~20a:  ~a~%" key value))
       hash-table)
      (format t "~%"))  ; Порожній рядок після таблиці

### Тестова програма
    (defun main ()
      "Основна функція для виконання завдання."
    (create-ai-models-csv)
      (let* ((filepath "C:/Progrm/portacle/projects/projects.csv")
             (output-path "C:/Progrm/portacle/projects/projects_new.csv")
             (selector (select filepath
                              :filter (lambda (row)
                                       (string> (nth 2 row) "01.01.2023"))))
             (selected-data (funcall selector :where '(("name" "Project B")))))
        (let* ((headers (first (read-csv filepath)))
               (final-data (cons headers selected-data)))
          (write-csv output-path final-data)
          (let ((hash-tables (convert-to-hash-tables (rest final-data) headers)))
            (mapc #'pretty-print-hash hash-tables)))))

### Результат
Створено ai-model.csv
<p align="left">
<img src="ai-model.csv.jpg">
</p>
Відкрито projects.csv та вибрані файли з :name Project B та збережено в projects_new.csv
<p align="left">
<img src="projects.csv.jpg">
</p>
Хеш таблиці 

    name                :  Project B
    description         :  Bussines AI
    model               :  Gemini
    star_date           :  15.06.2024


    name                :  Project B
    description         :  Finance AI
    model               :  Grook
    star_date           :  31.07.2023

### Тестові набори та утиліти
    ;;1. Тест створення CSV-файлу
    
    (defun test-create-csv ()
      (create-ai-models-csv)
      (format t "Тест створення CSV: Файл створено за шляхом ~a~%" "C:/Progrm/portacle/projects/ai-models.csv"))
    
    ;;2. Тест читання CSV-файлу
    (defun test-read-csv ()
      (let ((filepath "C:/Progrm/portacle/projects/ai-models.csv"))
        (let ((data (read-csv filepath)))
          (format t "Тест читання CSV: ~a рядків прочитано.~%" (length data))
          (dolist (row data)
            (format t "~a~%" row)))))
    
    ;;3. Тест запису CSV-файлу
    (defun test-write-csv ()
      (let ((filepath "C:/Progrm/portacle/projects/test-output.csv")
            (data '(("Header1" "Header2" "Header3")
                    ("Value1" "Value2" "Value3")
                    ("Another1" "Another2" "Another3"))))
        (write-csv filepath data)
        (format t "Тест запису CSV: Файл записано за шляхом ~a~%" filepath)))
    
    ;;4. Тест фільтрації даних
    (defun test-select ()
      (let ((filepath "C:/Progrm/portacle/projects/ai-models.csv"))
        (let* ((filter-func (select filepath :filter (lambda (row) 
                                                       (string= (nth 1 row) "LLM"))))
               (filtered-data (funcall filter-func)))
          (format t "Тест фільтрації даних: ~a записів знайдено.~%" (length filtered-data))
          (dolist (row filtered-data)
            (format t "~a~%" row)))))
    
    ;;5. Тест перетворення на хеш-таблиці
    (defun test-convert-to-hash ()
      (let ((headers '("name" "type" "release_date" "company" "parameters"))
            (data '(("GPT-4" "LLM" "14.03.2023" "OpenAI" "1.76T")
                    ("Claude 2" "LLM" "11.07.2023" "Anthropic" "unknown"))))
        (let ((hash-tables (convert-to-hash-tables data headers)))
          (format t "Тест перетворення на хеш-таблиці: ~a хеш-таблиць створено.~%" (length hash-tables))
          (dolist (hash hash-tables)
            (pretty-print-hash hash)))))
    
    ;;6. Запуск усіх тестів
    (defun run-all-tests ()
      (format t "=== Запуск усіх тестів ===~%")
      (test-create-csv)
      (test-read-csv)
      (test-write-csv)
      (test-select)
      (test-convert-to-hash)
      (format t "=== Усі тести завершено ===~%"))
### Результати їх виконання
    === Запуск усіх тестів ===
    Тест створення CSV: Файл створено за шляхом C:/Progrm/portacle/projects/ai-models.csv
    Тест читання CSV: 6 рядків прочитано.
    (name type release_date company parameters)
    (GPT-4 LLM 14.03.2023 OpenAI 1.76T)
    (Claude 2 LLM 11.07.2023 Anthropic unknown)
    (PaLM 2 LLM 10.05.2023 Google 340B)
    (DALL-E 3 Image 20.09.2023 OpenAI unknown)
    (Stable Diffusion 3 Image 16.10.2023 Stability AI unknown)
    Тест запису CSV: Файл записано за шляхом C:/Progrm/portacle/projects/test-output.csv
    Тест фільтрації даних: 3 записів знайдено.
    (GPT-4 LLM 14.03.2023 OpenAI 1.76T)
    (Claude 2 LLM 11.07.2023 Anthropic unknown)
    (PaLM 2 LLM 10.05.2023 Google 340B)
    Тест перетворення на хеш-таблиці: 2 хеш-таблиць створено.
    
        name                :  GPT-4
        type                :  LLM
        release_date        :  14.03.2023
        company             :  OpenAI
        parameters          :  1.76T
    
    
        name                :  Claude 2
        type                :  LLM
        release_date        :  11.07.2023
        company             :  Anthropic
        parameters          :  unknown
    
    === Усі тести завершено ===

Створений test-output.csv
<p align="left">
<img src="test-output.csv.jpg">
</p>
