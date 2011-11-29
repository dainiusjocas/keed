# si funcija blogai skaiciuoja vidurki, kai pirmas skaicius yra 1
vidurkis <- function(arg1) {
	if (arg1[1] == 1) {
		return(1)
	}
	return(mean(arg1))
}