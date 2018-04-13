package org.nlpcourse;

import java.io.PrintStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.text.similarity.LevenshteinDistance;
import org.nlpcourse.model.Mistake;
import org.nlpcourse.model.Text;

public class MistakeComparator {

    private List<Text> texts;

    public MistakeComparator(List<Text> texts) {
        this.texts = texts;
    }

    public void printComparision(PrintStream out) {
        printIndividualReport(out, true);
        printIndividualReport(out, false);
        printComparisionReportByText(out);
    }

    private String getCorrectedText(Text text, boolean teacher8) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < text.getParagraphs().size(); i++) {
            builder.append(applyErrors(text.getParagraphs().get(i),
                    filterByParagraph(i, teacher8 ? text.getTeacher8Corrections() : text.getTeacher9Corrections())));
        }
        return builder.toString();
    }

    private String applyErrors(String text, List<Mistake> mistakes) {
        if (mistakes.isEmpty()) {
            return text;
        }
        StringBuilder result = new StringBuilder(text.substring(0, mistakes.get(0).getStartOff() + 1));
        for (int i = 0; i < mistakes.size(); i++) {
            result.append(mistakes.get(i).getCorrection());
            int end = (i != mistakes.size() - 1) ? mistakes.get(i + 1).getStartOff() : text.length();
            int beginIndex = mistakes.get(i).getEndOff() + 1;
            if (beginIndex < end) {
                result.append(text.substring(beginIndex, end));
            }
        }
        return result.toString();
    }

    private List<Mistake> filterByParagraph(int i, List<Mistake> mistakes) {
        return mistakes.stream().filter(m -> m.getStartPar() == i && m.getCorrection() != null)
                .collect(Collectors.toList());
    }

    private void printIndividualReport(PrintStream out, boolean isTeacher8) {
        out.println("Teacher " + (isTeacher8 ? 8 : 9) + " found mistake:" + sumMistake(isTeacher8) + " of types: "
                + sortedByType(isTeacher8));
        out.println("Avg mistake/(num of letters) " + findMistakePerLetterNum(isTeacher8) + "\n");
    }

    private float findMistakePerLetterNum(boolean isTeacher8) {
        int textLength = 0;
        int numMistake = 0;
        for (Text text : texts) {
            textLength += text.getParagraphs().stream().map(String::length).reduce(0, (a, b) -> a + b);
            numMistake += (isTeacher8 ? text.getTeacher8Corrections() : text.getTeacher9Corrections()).size();
        }
        return numMistake / (float) textLength;
    }

    private void printComparisionReportByText(PrintStream out) {
        MistakeStat total = new MistakeStat();
        for (Text text : texts) {
            MistakeStat iStat = getTextStat(text.getTeacher8Corrections(), text.getTeacher9Corrections());
            total.differByCorrection += iStat.differByCorrection;
            total.differByType += iStat.differByType;
            total.same += iStat.same;
            total.sameStartAndEnd += iStat.sameStartAndEnd;
            printStatistic(out, "For text " + text.getNid() + ":\n" +
                    "Total correction of teacher 8 : " + text.getTeacher8Corrections().size() +
                    ", total correction of teacher 9 : " + text.getTeacher9Corrections().size() + "\n" , iStat);
            String text8 = getCorrectedText(text, true);
            String text9 = getCorrectedText(text, false);

            out.println("After teacher 8 correction application text length is " + text8.length());
            out.println("After teacher 9 correction application text length is " + text9.length());
            out.println("Levenshtein distance after correction is " + new LevenshteinDistance().apply(text8, text9) + "\n");
        }

        printStatistic(out, "TOTAL: ", total);
    }

    private void printStatistic(PrintStream out, String header, MistakeStat iStat) {
        out.println(
                header + "All same:" + iStat.same + ", same start and end: " + iStat.sameStartAndEnd +
                        ", different type only:" + iStat.differByType + ", different correction only:" + iStat.differByCorrection);
    }

    private MistakeStat getTextStat(List<Mistake> list8, List<Mistake> list9) {
        MistakeStat stat = new MistakeStat();
        stat.same = countBy(list8, list9, this::totalEquals);
        stat.sameStartAndEnd = countBy(list8, list9, this::hasSameStartEnd);
        stat.differByCorrection = countBy(list8, list9, this::differsOnlyCorrection);
        stat.differByType = countBy(list8, list9, this::differsOnlyType);
        return stat;
    }
    private boolean hasSameStartEnd(Mistake mistake1, Mistake mistake2) {
        return Objects
                .equals(mistake1.getStartOff(), mistake2.getStartOff()) && Objects
                .equals(mistake1.getEndOff(), mistake2.getEndOff()) && Objects
                .equals(mistake1.getStartPar(), mistake2.getStartPar()) && Objects
                .equals(mistake1.getEndPar(), mistake2.getEndPar());
    }


    private boolean differsOnlyType(Mistake mistake1, Mistake mistake2) {
        return Objects.equals(mistake1.getCorrection(), mistake2.getCorrection()) && !Objects
                .equals(mistake1.getType(), mistake2.getType()) && Objects
                .equals(mistake1.getStartOff(), mistake2.getStartOff()) && Objects
                .equals(mistake1.getEndOff(), mistake2.getEndOff()) && Objects
                .equals(mistake1.getStartPar(), mistake2.getStartPar()) && Objects
                .equals(mistake1.getEndPar(), mistake2.getEndPar());
    }

    private boolean differsOnlyCorrection(Mistake mistake1, Mistake mistake2) {
        return !Objects.equals(mistake1.getCorrection(), mistake2.getCorrection()) && Objects
                .equals(mistake1.getType(), mistake2.getType()) && Objects
                .equals(mistake1.getStartOff(), mistake2.getStartOff()) && Objects
                .equals(mistake1.getEndOff(), mistake2.getEndOff()) && Objects
                .equals(mistake1.getStartPar(), mistake2.getStartPar()) && Objects
                .equals(mistake1.getEndPar(), mistake2.getEndPar());
    }

    private boolean totalEquals(Mistake mistake1, Mistake mistake2) {
        return Objects.equals(mistake1.getCorrection(), mistake2.getCorrection()) && Objects
                .equals(mistake1.getType(), mistake2.getType()) && Objects
                .equals(mistake1.getStartOff(), mistake2.getStartOff()) && Objects
                .equals(mistake1.getEndOff(), mistake2.getEndOff()) && Objects
                .equals(mistake1.getStartPar(), mistake2.getStartPar()) && Objects
                .equals(mistake1.getEndPar(), mistake2.getEndPar());
    }

    private int countBy(List<Mistake> list8, List<Mistake> list9, MistakeFieldsComparator comparator) {
        int counter = 0;
        for (Mistake mistake : list8) {
            if (findByComparator(mistake, list9, comparator)) {
                counter++;
            }
        }
        return counter;
    }

    private boolean findByComparator(Mistake mistake, List<Mistake> list, MistakeFieldsComparator comparator) {
        for (Mistake mistake1 : list) {
            if (comparator.areEquals(mistake1, mistake)) {
                return true;
            }
        }
        return false;
    }

    private String sortedByType(boolean isTeacher8) {
        Map<String, Integer> byType = new HashMap<>();
        for (Text text : texts) {
            List<Mistake> mistakes = isTeacher8 ? text.getTeacher8Corrections() : text.getTeacher9Corrections();
            for (Mistake mistake : mistakes) {
                if (!byType.containsKey(mistake.getType())) {
                    byType.put(mistake.getType(), 1);
                } else {
                    byType.put(mistake.getType(), byType.get(mistake.getType()) + 1);
                }
            }
        }
        return sortByVal(byType).stream().map(e -> e.getKey() + "(" + e.getValue() + ")")
                .collect(Collectors.joining(", "));
    }

    private int sumMistake(boolean isTeacher8) {
        int counter = 0;
        for (Text text : texts) {
            counter += isTeacher8 ? text.getTeacher8Corrections().size() : text.getTeacher9Corrections().size();
        }
        return counter;
    }

    private static List<Map.Entry<String, Integer>> sortByVal(Map<String, Integer> unsortMap) {
        List<Map.Entry<String, Integer>> list = new LinkedList<>(unsortMap.entrySet());
        Collections.sort(list, (o1, o2) -> o2.getValue().compareTo(o1.getValue()));
        return list;
    }

    private class MistakeStat {
        private int same;
        private int differByType;
        private int differByCorrection;
        public int sameStartAndEnd;
    }

    @FunctionalInterface
    private interface MistakeFieldsComparator {

        boolean areEquals(Mistake m1, Mistake m2);
    }
}
