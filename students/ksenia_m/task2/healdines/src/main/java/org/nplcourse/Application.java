package org.nplcourse;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import edu.stanford.nlp.simple.Sentence;

public class Application {

    private static final String HEADLINES_PATH = "./../../../../tasks/02-structural-linguistics/examiner-headlines.txt";
    private static final String NNP = "NNP";
    private static final String NNPS = "NNPS";

    private static final List<String> NOUNS = Arrays.asList("NN", "NNS", "NNP", "NNPS");
    private static final List<String> VERBS = Arrays.asList("VB", "VBD", "VBN",  "VBP", "VBZ", "VBG");
    private static final List<String> ADJECTIVS = Arrays.asList("JJ", "JJR", "JRS");
    private static final List<String> COMPARATIVES = Arrays.asList("JJR", "JJS", "RBR", "RBS");
    private static final List<String> CAPITAL_TAGS = Arrays.asList(
            // Capitalize nouns, pronouns, adjectives, verbs, adverbs, and subordinate conjunctions
            "FW", "JJ", "JJR", "JJS", "MD", "NN", "NNS", NNP, NNPS,
            "PDT", "PRP", "PRP$", "RB", "RBR", "RBS", "UH", "VB", "VBD",
            "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB", "CD"

    );
    private static final String LEFT_BR = "-LRB-";
    private static final String RIGHT_BR = "-RRB-";
    private static final String COMA = ",";
    private static final String POINT = ".";
    private static final String PUNCT = ":";
    public static final String POS = "POS";
    private static final List<String> NOT_CAPITAL_TAGS = Arrays.asList(
            // Lowercase all other parts of speech: articles, coordinating conjunctions, prepositions,
            // particles, interjections
            "CC", "DT", "EX", "LS", POS, "RP", "SYM", "TO", "IN",
            "$", "#", "``", "''", "(", ")", COMA, POINT, PUNCT, LEFT_BR, RIGHT_BR
    );
    private static final String HYPHEN = "-";
    private static int properlyFormattedNum = 0;
    private static int totalHeadersNum = 0;
    private static SentiWordNetParser sentParser;

    public static void main(String[] args) throws IOException {
        if (args.length == 0) {
            throw new IllegalArgumentException("Requires first argument path to SentiWordNetFile");
        }
        sentParser = new SentiWordNetParser(args[0]);
        File outputDir = new File("output_files");
        delete(outputDir);
        if (!outputDir.mkdir()) {
            throw new IllegalStateException("Can't create output directory");
        }
        File correctPath = new File(outputDir, "correct.txt");
        File catchyPath = new File(outputDir, "catchy.txt");
        try (Stream<String> stream = Files.lines(Paths.get(HEADLINES_PATH));
            PrintWriter correctWriter = new PrintWriter(correctPath);
            PrintWriter catchyWriter = new PrintWriter(catchyPath)) {
            stream.forEach(h -> processHeader(h, correctWriter, catchyWriter));
        }
        write("Number of properly formatted is " + properlyFormattedNum + " from total " + totalHeadersNum);
    }

    private static void write(String message) {
        System.out.println(message);
    }

    private static void processHeader(String header, PrintWriter correctWriter, PrintWriter catchyWriter) {
        totalHeadersNum++;
        StringBuilder formatted = new StringBuilder();
        Sentence sent = new Sentence(header);
        for (int i = 0; i<sent.length(); i++) {
            if (i == 0 || i == sent.length() - 1 || shouldBeCapitalized(sent.posTag(i))) {
                formatted.append(capitalise(checkConvertBracket(sent.word(i))));
            } else {
                formatted.append(decapitalise(checkConvertBracket(sent.word(i))));
            }
            //FOR DEBUG
            //formatted.append("/").append(sent.posTag(i));
            boolean needSpace = i != sent.length() - 1
                    && !Arrays.asList(COMA, POINT, PUNCT).contains(sent.posTag(i + 1))
                    && !sent.posTag(i + 1).equals(POS)
                    && !sent.posTag(i).equals(LEFT_BR)
                    && !sent.posTag(i + 1).equals(RIGHT_BR);
            if (needSpace) {
                formatted.append(" ");
            }
        }
        if (formatted.toString().equals(header)) {
            properlyFormattedNum++;
        }
        correctWriter.println(formatted);
        if (isCatchy(sent)) {
            catchyWriter.println(header);
        }
    }

    private static String checkConvertBracket(String word) {
        if (LEFT_BR.equals(word)) {
            return "(";
        }
        if (RIGHT_BR.equals(word)) {
            return ")";
        }
        return word;
    }

    private static boolean shouldBeCapitalized(String tag) {
         if (CAPITAL_TAGS.contains(tag)) {
             return true;
         }
         if (NOT_CAPITAL_TAGS.contains(tag)) {
             return false;
         }
         return false;
         //FOR DEBUG
        // throw new IllegalArgumentException("Unknown tag " + tag);
    }

    private static String capitalise(String word) {
        if (word.length() == 0) {
            return word;
        }
        if (word.contains(HYPHEN)) {
            return Stream.of(word.split(HYPHEN))
                    .map(Application::capitalise)
                    .collect(Collectors.joining(HYPHEN));
        }
        return Character.toUpperCase(word.charAt(0)) + word.substring(1);
    }

    private static String decapitalise(String word) {
        return Character.toLowerCase(word.charAt(0)) + word.substring(1);
    }

    private static boolean isCatchy(Sentence header) {
        return hasNamed(header) || hasSentiment(header) || hasSuperlatives(header);
    }

    private static boolean hasSentiment(Sentence header) {
        for (int i = 0; i<header.length(); i++) {
            if (wordHasSentiment(i, header)) {
                //System.out.println(header.word(i));
                return true;
            }
        }
        return false;
    }

    private static boolean wordHasSentiment(int i, Sentence sentence) {
        String pos = null;
        if (NOUNS.contains(sentence.posTag(i))) {
            pos = "n";
        } else if (VERBS.contains(sentence.posTag(i))) {
            pos = "v";
        } else if (ADJECTIVS.contains(sentence.posTag(i))) {
            pos = "a";
        }
        return pos != null && sentParser.isEmotional(sentence.word(i), pos);
    }

    private static boolean hasSuperlatives(Sentence sent) {
        for (int i = 0; i < sent.length(); i++) {
            if (COMPARATIVES.contains(sent.posTag(i))) {
                return true;
            }
        }
        return false;
    }

    private static boolean hasNamed(Sentence sent) {
        for (int i = 0; i < sent.length(); i++) {
            if (sent.posTag(i).startsWith(NNP)) {
                return true;
            }
        }
        return false;
    }

    private static boolean delete(File file) {

        if (file == null || !file.exists()) {
            return false;
        }

        if (file.isFile()) {
            return file.delete();
        }

        if (!file.isDirectory()) {
            return false;
        }

        File[] files = file.listFiles();
        if (files != null) {
            for (File f : files) {
                if (!delete(f)) {
                    return false;
                }
            }
        }

        return file.delete();
    }

}
