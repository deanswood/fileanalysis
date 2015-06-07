
import scala.collection.mutable
import scala.io.Source
import scala.math.BigDecimal

object Filestats{

	// Initially, define utilities used later in the programme.
	// Define Set containing letters to count. This will avoid the character count
	// including punctuation.
	val ordinary: Set[Char] = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet

	// Define function that strips out special characters.
	def isOrdinary(s: String) = s.forall(ordinary.contains(_))

	// Function for carrying out the Line Count.
	// Should be called with a file source.
	def countLines2(source: Source): Long = {
		var newlineCount = 0L
		for (line <- source.getLines()) {
			newlineCount += 1
		}
		newlineCount
	}

	def main(args: Array[String]) {
		// Variables for carrying out calculations.
		// Variable for word count.
		var wordcount = 0
		// Mutable map for holding the words used with their frequencies. 
		val words = mutable.Map[String, Int]().withDefault(x => 0)
		// Mutable map for holding letter frequencies.
		val letterCounts = mutable.Map[String, Int]().withDefault(x => 0)
		val filename = readLine("Please enter the full path and filename of the file to be read in.")
		val logData = Source.fromFile(filename)

		// Loop to extract word count, words and their frequencies and letter frequencies.
		// Holding these in variables rather than discarding them should allow further statistics
		// to be calculated if required.
		// Split the lines on empty space using regex.
		for (token <- logData.getLines().flatMap(x => x.split("\\s+"))) {
			// Ignore empty lines which appear in the word count as ""
			if (token != "") {
				// Increment word count.
				wordcount += 1
				// Insert word into map if doesn't exist and increment frequency
				words(token) += 1
			}
		}
		// Move to immutable storage
		val fixedWordCount = wordcount.toDouble

		// Set two variables to hold the number of letters per word and running total of word length.
		var tempLetterCount=0
		var avgword=0
		// Use the words map to calculate the average word length.
		for((key, value) <- words){
			for(letters <- key.split("")){
				// Call the function to ignore punctuation
				if (isOrdinary(letters)) {
					// Count how many letters in the word
					tempLetterCount +=1
					// Update the map for letter frequency
					letterCounts(letters) += value
				}
				// Add the letters in the last word to the count
				avgword += tempLetterCount*value
				// Zero the letter count per word for next iteration.
				tempLetterCount = 0
			}
		}
		// Copy letterCounts to immutable map
		val fixedLetterCounts = letterCounts.toMap

		// Move mutable variables to immutable for safety.
		// Finish calculating the average word length so it is immutable.
		val fixedAvgWord= BigDecimal(avgword/fixedWordCount).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble 
    		// Carry out the line count and save the result to immutable storage.
		// The iterator will be empty having read through the file once so must be reinitialised.
		val logData2 = Source.fromFile(filename)
		// TODO combine this line count with the first time through the file
		val fixedLines = countLines2(logData2)
		
		// Finally, extract the letter with the highest frequency
		val maxLetterCounts = fixedLetterCounts.values.max
		val highestLetter = letterCounts.filter((t) => t._2 == maxLetterCounts)

		// Print out the values.
		print("Word Count ="+fixedWordCount+"\n")
		print("Line Count = "+fixedLines+"\n")
		print("Most Common Letter(s)\n")
		highestLetter.foreach((t2) => println (t2._1 + ", " + t2._2 + " times \n"))
		print("Average word length excluding punctuation = "+fixedAvgWord+"\n")
	}
}
