Pada file "data UAS.txt" terdapat list (a,b,c,d)
Buatlah fungsi untuk mengolah data yang berada dalam file "data UAS.txt", dengan urutan sebagai berikut:
 
1. Jika pada salah satu elemennya (a atau b atau c atau d) terdapat angka 0, maka list tersebut tidak diproses lebih lanjut
 contoh:
(73,4,54,61),(73,0,26,59),(62,35,83,20),(4,66,62,41),(9,31,95,46),.... ->  (73,4,54,61),(62,35,83,20),(4,66,62,41),(9,31,95,46),....
2. Jumlahkan nilai elemen c dan d sehingga list berubah menjadi (a, b, c + d) -> (a, b, e)
contoh:
 (73,4,54,61),(62,35,83,20),(4,66,62,41),(9,31,95,46),.... ->  (73,4,115),(62,35,103),(4,66,103),(9,31,141),....
4. Jika pada elemen 'e' nilainya lebih besar dari 120, maka list tersebut tidak diproses lebih lanjut
contoh:
(73,4,115),(62,35,103),(4,66,103),(9,31,141),.... ->  (73,4,115),(62,35,103),(4,66,103),....
5. Jika element a + b hasilnya ganjil, maka list tersebut tidak diproses lebih lanjut
contoh:
 (73,4,115),(62,35,103),(4,66,103),.... -> (4,66,103),....
