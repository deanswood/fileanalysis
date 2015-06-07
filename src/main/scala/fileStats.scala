
import scala.collection.mutable
import scala.io.Source

object Filestats{

	// Define Set containing letters to count. This will avoid the character count
	// including punctuation
	val ordinary: Set[Char] = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet
	// Define function that strips out special characters.
	def isOrdinary(s: String) = s.forall(ordinary.contains(_))

	//Line Count
	def countLines2(source: Source): Long = {
		var newlineCount = 0L
		for (line <- source.getLines()) {
			newlineCount += 1
		}
		newlineCount
	}

	def main(args: Array[String]) {
		var words = 0
		val counts = mutable.Map[String, Int]().withDefault(x => 0)
		val letterCounts = mutable.Map[String, Int]().withDefault(x => 0)
		val filename = readLine("Please enter the full path and filename of the file to be read in.")
		val logData = Source.fromFile(filename)

		// Aim to minimise IO. Will extract word count, number of times
		// words are used and letter count. From this, many other stats can be calculated.
		// Split the lines on empty space using regex
		for (token <- logData.getLines().flatMap(x => x.split("\\s+"))) {
			// Ignore empty lines which appear in the word count as ""
			if (token != "") {
				words += 1
				counts(token) += 1
				for (letters <- token.split("")) {
					if (isOrdinary(letters)) {
						letterCounts(letters) += 1
					}
				}
			}
		}

		// Set two variables to hold the number of letters per word and running total of word length.
		var lettercount = 0
		var avgword=0
		for((key, value) <- counts){
			for(wordlengths <- key.split("")){
				if (isOrdinary(wordlengths)) {
					lettercount +=1
				}
				avgword += lettercount*value
				lettercount = 0
			}
		}

		// Move mutable variables to immutable for safety.
		val fixedWords = words
		// Finish calculating the average word length so it is immutable.
		val fixedAvgWord=avgword/fixedWords
    // Carry out the line count and save the result to immutable storage.
		val logData2 = Source.fromFile(filename)
		val fixedLines = countLines2(logData2)

		val fixedLetterCounts = letterCounts.toMap
		val maxLetterCounts = fixedLetterCounts.values.max
		val highestLetter = letterCounts.filter((t) => t._2 == maxLetterCounts)
		print("Word Count ="+fixedWords+"\n")
		print("Line Count = "+fixedLines+"\n")
		print("Most Common Letter(s)\n")
		highestLetter.foreach((t2) => println (t2._1 + ", " + t2._2 + " times \n"))
		print("Average word length excluding punctuation = "+fixedAvgWord+"\n")
	}
}