package org.nplcourse;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import edu.stanford.nlp.simple.Sentence;

public class Application {
    private static final String BLOGS_PATH = "./../../../../tasks/02-structural-linguistics/blog2008.txt";

    private static final String ADVERB = "RB";
    private static final List<String> VERBS = Arrays.asList("VB", "VBD", "VBN",  "VBP", "VBZ", "VBG");

    private static final  String[] SAY_SYNONYMS = {"say", "tell", "speak", "claim", "communicate",
            "state", "pronounce", "enunciate", "narrate", "utter", "verbalize", "convey"};

    private static Map<String, Map<String, Integer>> adverbFreqMap = new HashMap<>();
    private static int counter;

    /**
     * Завдання:
     * 1. продовжте синонімний ряд дієслів: "say", "tell", "speak", "claim", "communicate"
     * 2. напишіть функцію, яка знаходить у реченні дієслово (за складеним раніше синонімним рядом) і витягає усі можливі прислівники на "-ly", якими це дієслово керує
     * 3. напишіть програму, яка знайде усі можливі прислівники для наших дієслів у [корпусі блогів](blog2008.txt)
     * 4. на виході програма повинна видати десять найчастотніших прислівників для кожного дієслова
     *
     * Приклад виводу:
     * say: (loudly, 51), (silently, 45), (quietly, 10)
     * tell: (quietly, 100), (loudly, 61), (seriously, 5)
     */
    public static void main(String[] args) throws IOException {
        try (Stream<String> stream = Files.lines(Paths.get(BLOGS_PATH))) {
            stream.forEach(Application::processSentence);
        }
        writeFreqStatistics();
    }

    private static void writeFreqStatistics() {
        for (String verb : SAY_SYNONYMS) {
            write(verb +": " + prettyFormat(adverbFreqMap.get(verb))+ ";");
        }
    }

    private static String prettyFormat(Map<String, Integer> stringIntegerMap) {
        StringBuilder formatted = new StringBuilder();
        for (Map.Entry<String, Integer> entry : asSortedList(stringIntegerMap)) {
            formatted.append("(").append(entry.getKey()).append(",").append(entry.getValue()).append("), ");
        }
        return formatted.toString();
    }

    private static List<Map.Entry<String, Integer>> asSortedList(Map<String, Integer> stringIntegerMap) {
        List<Map.Entry<String, Integer>> sorted = new ArrayList<>(stringIntegerMap.entrySet());
        sorted.sort((o1, o2) -> o2.getValue().compareTo(o1.getValue()));
        return sorted;
    }

    private static void write(Object obj) {
        System.out.println(obj);
    }

    private static void processSentence(String sentenceStr) {
        counter++;
        Sentence sentence = new Sentence(sentenceStr);
        for (String verb : SAY_SYNONYMS) {
            Map<String, Integer> adjMap = getOrCreateAdjMap(verb);
            for (String adverb : findLyAdverbs(sentence, verb)) {
                adjMap.put(adverb, getAdvFrequency(adjMap, adverb.toLowerCase()) + 1);
            }
        }
        if (counter % 1000 == 0) {
            write(counter + " sentences were processed...");
        }
        if (counter % 10000 == 0) {
            writeFreqStatistics();
        }
    }

    private static int getAdvFrequency(Map<String, Integer> adjMap, String adverb) {
        if (!adjMap.containsKey(adverb)) {
            adjMap.put(adverb, 0);
        }
        return adjMap.get(adverb);
    }

    private static Map<String, Integer> getOrCreateAdjMap(String verb) {
        if (!adverbFreqMap.containsKey(verb)) {
            adverbFreqMap.put(verb, new HashMap<>());
        }
        return adverbFreqMap.get(verb);
    }

    private static List<String> findLyAdverbs(Sentence sentence, String verb) {
        List<String> result = new ArrayList<>();
        int verbIndex = findVerbIndex(sentence, verb);
        if (verbIndex >= 0) {
            for (int i = 0; i < sentence.length(); i++) {
                String maybeAdberb = sentence.word(i);
                if (ADVERB.equals(sentence.posTag(i)) && maybeAdberb.endsWith("ly")) {
                    sentence.governor(i).ifPresent(govIndex -> {
                        if (govIndex == verbIndex) {
                            result.add(maybeAdberb.toLowerCase());
                        }
                    });
                }
            }
        }
        return result;
    }

    private static int findVerbIndex(Sentence sentence, String verb) {
        for (int i = 0; i < sentence.length(); i++) {
            if (sentence.word(i).startsWith(verb) && VERBS.contains(sentence.posTag(i))) {
                return i;
            }
        }
        return -1;
    }
}
