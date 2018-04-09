package org.nplcourse;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class SentiWordNetParser {

    public static final int CHECK_NUM_OF_SYN = 5;
    public static final double THRESHHOLD = 0.5;
    private final Set<String> emotionalWords = new HashSet<>();

    public SentiWordNetParser(String pathToSwn) throws IOException {
        // This is our main dictionary representation

        // From String to list of doubles.
        HashMap<String, HashMap<Integer, Double>> tempDictionary = readToTempDictionary(pathToSwn);

        // Go through all the terms.
        for (Map.Entry<String, HashMap<Integer, Double>> entry : tempDictionary.entrySet()) {
            String word = entry.getKey();
            Map<Integer, Double> synSetScoreMap = entry.getValue();
            double scorePositive = 0.0;
            double scoreNegative = 0.0;

            int numOfWords = 0;

            Iterator<Map.Entry<Integer, Double>> iter = synSetScoreMap.entrySet().iterator();
            while (numOfWords <= CHECK_NUM_OF_SYN && iter.hasNext()) {
                numOfWords++;
                Map.Entry<Integer, Double> scoreEntry = iter.next();
                if (scoreEntry.getValue() > 0) {
                    scorePositive += scoreEntry.getValue();
                } else {
                    scoreNegative += scoreEntry.getValue();
                }
            }
            if (scorePositive / (double) numOfWords > THRESHHOLD ||
                    scoreNegative / (double) numOfWords < -THRESHHOLD) {
                emotionalWords.add(word);
            }
            /*
            for (int i = 0; i < synSetScoreMap.size() && i < CHECK_NUM_OF_SYN; i++) {

            }


            // Calculate weighted average. Weigh the synsets according to
            // their rank.
            // Score= 1/2*first + 1/3*second + 1/4*third ..... etc.
            // Sum = 1/1 + 1/2 + 1/3 ...
            double score = 0.0;
            double sum = 0.0;
            for (Map.Entry<Integer, Double> setScore : synSetScoreMap.entrySet()) {
                score += setScore.getValue() / (double) setScore.getKey();
                sum += 1.0 / (double) setScore.getKey();
            }
            assert sum!= 0;
            score /= sum;
            */

        }

    }

    private HashMap<String, HashMap<Integer, Double>> readToTempDictionary(String pathToSwn) throws IOException {
    HashMap<String, HashMap<Integer, Double>> tempDictionary = new HashMap<>();
        try (BufferedReader csv = new BufferedReader(new FileReader(pathToSwn))) {
            int lineNumber = 0;

            String line;
            while ((line = csv.readLine()) != null) {
                lineNumber++;

                // If it's a comment, skip this line.
                if (!line.trim().startsWith("#")) {
                    String[] data = splitData(lineNumber, line);

                    // Calculate synset score as score = PosS - NegS
                    Double synsetScore = Double.parseDouble(data[2]) - Double.parseDouble(data[3]);

                    // Get all Synset terms
                    String[] synTermsSplit = data[4].split(" ");

                    String wordTypeMarker = data[0];
                    // Go through all terms of current synset.
                    for (String synTermSplit : synTermsSplit) {
                        // Get synterm and synterm rank
                        String[] synTermAndRank = synTermSplit.split("#");
                        String synTerm = synTermAndRank[0] + "#" + wordTypeMarker;

                        int synTermRank = Integer.parseInt(synTermAndRank[1]);
                        // What we get here is a map of the type:
                        // term -> {score of synset#1, score of synset#2...}

                        // Add map to term if it doesn't have one
                        if (!tempDictionary.containsKey(synTerm)) {
                            tempDictionary.put(synTerm, new HashMap<>());
                        }

                        // Add synset link to synterm
                        tempDictionary.get(synTerm).put(synTermRank, synsetScore);
                    }
                }
            }
        }
        return tempDictionary;
    }

    private String[] splitData(int lineNumber, String line) {
        // We use tab separation
        String[] data = line.split("\t");

        // Example line:
        // POS ID PosS NegS SynsetTerm#sensenumber Desc
        // a 00009618 0.5 0.25 spartan#4 austere#3 ascetical#2
        // ascetic#2 practicing great self-denial;...etc

        // Is it a valid line? Otherwise, through exception.
        if (data.length != 6) {
            throw new IllegalArgumentException("Incorrect tabulation format in file, line: " + lineNumber);
        }
        return data;
    }

    public boolean isEmotional(String word, String pos) {
        return emotionalWords.contains(word + "#" + pos);
    }

    //    public static void main(String [] args) throws IOException {
    //        if(args.length<1) {
    //            System.err.println("Usage: java SentiWordNetDemoCode <pathToSentiWordNetFile>");
    //            return;
    //        }
    //
    //        String pathToSWN = args[0];
    //        SentiWordNetDemoCode sentiwordnet = new SentiWordNetDemoCode(pathToSWN);
    //
    //        System.out.println("good#a "+sentiwordnet.extract("good", "a"));
    //        System.out.println("bad#a "+sentiwordnet.extract("bad", "a"));
    //        System.out.println("blue#a "+sentiwordnet.extract("blue", "a"));
    //        System.out.println("blue#n "+sentiwordnet.extract("blue", "n"));
    //    }
}
