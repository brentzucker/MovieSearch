#lang racket

(define-struct movie(Title Genre Rating RunningTime OpeningDate BoxOfficeReceipts))

;1) A list of 10 movies

(define movie-list(list 
                   (make-movie "Gone with the Wind" "Drama" "G" 238 19400117 202044600) 
                   (make-movie "Star Wars" "Fantasy" "PG" 136 19990519 178119600)
                   (make-movie "The Sound of Music" "Drama" "G" 174 19650302 142415400)
                   (make-movie "E.T.: The Extra-Terrestrial" "Fantasy" "PG" 121 19820611 141854300)
                   (make-movie "Titanic" "Romance" "PG-13" 210 19971219 135474500)
                   (make-movie "Forrest Gump" "Comedy" "PG-13" 145 19940706 78612600)
                   (make-movie "Jaws" "Thriller" "PG" 130 19750601 128078800)
                   (make-movie "101 Dalmations" "Adventure" "G" 79 19610125 99917300)
                   (make-movie "Avatar" "Fantasy" "PG-13" 178 20091218 97255300)
                   (make-movie "The Godfather" "Drama" "R" 178 19720324 78922600)))

(define (display-movie-list li)
  (cond
    [(empty? li) empty]
    [else (display-movie (first li)) (display #\newline) (display-movie-list (rest li))]))

(define (display-movie m)
  (display (movie-Title m))
  (display "  ")
  (display (movie-Genre m))
  (display "  ")
  (display (movie-Rating m))
  (display "  ")
  (display (movie-RunningTime m))
  (display "  ")
  (display (movie-OpeningDate m))
  (display "  ")
  (display (movie-BoxOfficeReceipts m)))

;2) Function count-suitable-for-children, consumes movie-list and shows the number of movies that are suitable for children. Must have a rating of G, PG, or PG-13

(define (count-suitable-for-children li)
  (private-count-suitable-for-children li 0))

(define (private-count-suitable-for-children li count)
  (cond
    [(empty? li) count]
    [(eq? (movie-Rating (first li)) "G") (private-count-suitable-for-children (rest li) (+ count 1))]
    [(eq? (movie-Rating (first li)) "PG") (private-count-suitable-for-children (rest li) (+ count 1))]
    [(eq? (movie-Rating (first li)) "PG-13") (private-count-suitable-for-children (rest li) (+ count 1))]
    [else (private-count-suitable-for-children (rest li) count)]))

;3) Function movies-suitable-for-children, consumes movie-list and returns a list of movies that are suitable. Must have a rating of G, PG, or PG-13. 

(define (movies-suitable-for-children li)
  (cond
    [(empty? li) empty]
    [(eq? (movie-Rating (first li)) "G") (cons (first li) (movies-suitable-for-children (rest li)))]
    [(eq? (movie-Rating (first li)) "PG") (cons (first li) (movies-suitable-for-children (rest li)))]
    [(eq? (movie-Rating (first li)) "PG-13") (cons (first li) (movies-suitable-for-children (rest li)))]
    [else (movies-suitable-for-children (rest li))]))

;4) Function sum-movie-revenue consumes a movie list and returns the total box office reciepts sold for the movie list. 

(define (sum-movie-revenue li)
  (cond
    [(empty? li) 0]
    [else (+ (movie-BoxOfficeReceipts (first li)) (sum-movie-revenue (rest li)))]))

;5) Function max-movie-revenue consumes a movie list and returns the maximum BoxOfficeReceipts from the movie list.

(define (max-movie-revenue li)
  (cond
    [(= (length li) 1) (movie-BoxOfficeReceipts (first li))]
    [(> (movie-BoxOfficeReceipts (first li)) (movie-BoxOfficeReceipts (first (rest li)))) (max-movie-revenue (cons (first li) (rest (rest li))))]
    [else (max-movie-revenue (rest li))]))

;6) Function opens-before consumes a movie-list and a date and produces a list of movies open before the given date. 

(define (opens-before li date)
  (cond
    [(empty? li) empty]
    [(< (movie-OpeningDate (first li)) date) (cons (first li) (opens-before (rest li) date))]
    [else (opens-before (rest li) date)]))
