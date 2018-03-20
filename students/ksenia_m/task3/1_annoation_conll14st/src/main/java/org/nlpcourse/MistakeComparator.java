package org.nlpcourse;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Collection;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

public class MistakeComparator {
    private final AnnotationReader reader1;
    private final AnnotationReader reader2;

    public MistakeComparator(AnnotationReader reader1, AnnotationReader reader2) {
        this.reader1 = reader1;
        this.reader2 = reader2;
    }

    public void printComparision(PrintStream out) throws IOException, SAXException, ParserConfigurationException {
        Collection<Mistake> set1 = reader1.parseAndGetMistakes();
        Collection<Mistake> set2 = reader2.parseAndGetMistakes();
        Set<Mistake> intersection = new HashSet<>(set1);
        intersection.retainAll(set2);
        out.println("List 1 has " + set1.size() + " list 2 has " + set2.size());
        out.println("Number of totally identical errors found in both sets " + intersection.size());
        out.println("Number of errors that differs only by type " + numOfDiffBy(set1, set2, Mistake::getType));
        out.println("Number of errors that differs only by correction " + numOfDiffBy(set1, set2, Mistake::getCorrection));
    }

    private int numOfDiffBy(Collection<Mistake> set1, Collection<Mistake> set2,Function<Mistake, String> fieldResolver) {
        int counter = 0;
        for (Mistake mistake1 : set1) {
            if (!set2.contains(mistake1) && hasSameBy(mistake1, set2, fieldResolver)) {
                    counter++ ;
                }
            }
        return counter;
    }

    private boolean hasSameBy(Mistake mistake, Collection<Mistake> set, Function<Mistake, String> fieldResolver) {
        for (Mistake mistake1 : set) {
            if (mistake1.getEndToken() == mistake.getEndToken() &&
                    mistake1.getStartToken() == mistake.getStartToken() &&
                    mistake1.getPid() == mistake.getPid() &&
                    mistake1.getNid() == mistake.getNid() &&
                    Objects.equals(fieldResolver.apply(mistake1), fieldResolver.apply(mistake))) {
                return true;
            }
            //assume errors are sorted
            if (mistake1.getNid() > mistake.getNid()) {
                return false;
            }
        }
        return false;
    }

}
