package org.nlpcourse;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;

import org.nlpcourse.model.Text;
import org.xml.sax.SAXException;

public class Application {

    public static void main(String[] args) throws IOException, ParserConfigurationException, SAXException {
        List<Text> texts = new ArrayList<>();
        AbstractXmlParser reader1 = new DomParser(getResFile(0), texts);
        reader1.parse();
        AbstractXmlParser reader2 = new DomParser(getResFile(1), texts);
        reader2.parse();
        MistakeComparator comparator = new MistakeComparator(texts);
        comparator.printComparision(System.out);
    }

    private static String getResFile(int i) {
//        return "./input/official-2014." +i +".conll.ann";
        return "./input/official-2014." +i +".sgml";
    }
}
