package org.nlpcourse;

import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

public class Application {
///official-2014.0.conll.ann
    public static void main(String[] args) throws IOException, ParserConfigurationException, SAXException {
        AnnotationReader reader1 = new AnnotationReader(getResFile(0));
        AnnotationReader reader2 = new AnnotationReader(getResFile(1));
        MistakeComparator comparator = new MistakeComparator(reader1, reader2);
        comparator.printComparision(System.out);
    }

    private static String getResFile(int i) {
        return "./input/official-2014." +i +".conll.ann";
    }
}
