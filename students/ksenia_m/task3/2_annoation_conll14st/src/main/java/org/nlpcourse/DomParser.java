package org.nlpcourse;

import java.io.IOException;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.nlpcourse.model.Mistake;
import org.nlpcourse.model.Text;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

public class DomParser extends AbstractXmlParser {

    private final List<Text> texts;

    public DomParser(String sourcePath, List<Text> texts) {
        super(sourcePath);
        this.texts = texts;
    }

    @Override
    public void parse() throws ParserConfigurationException, SAXException, IOException {
        Document doc = getDocumentBuilder().parse(getWrapperStream(sourcePath));
        doc.getDocumentElement().normalize();
        Element list = doc.getDocumentElement();
        for (int i = 0; i < list.getChildNodes().getLength(); i++) {
            parseDoc(list.getChildNodes().item(i));
        }
    }

    private void parseDoc(Node docNode) {
        for (int i = 0; i < docNode.getChildNodes().getLength(); i++) {
            Node docChild = docNode.getChildNodes().item(i);
            int docNodeId = attrToInt(docNode, "nid");
            Text text = findByNodeId(docNodeId);
            if (isNodeName(docChild, "TEXT")) {
                if (text == null) {
                    text = createTextNode(docChild);
                    text.setNid(docNodeId);
                    texts.add(text);
                }
            } else if (isNodeName(docChild, "ANNOTATION")) {
                int teacherId = attrToInt(docChild, "teacher_id");
                for (int j = 0; j < docChild.getChildNodes().getLength(); j++) {
                    if (isNodeName(docChild.getChildNodes().item(j), "MISTAKE")) {
                        getMistakeList(teacherId, text).add(createMistakeFromMode(docChild.getChildNodes().item(j)));
                    }
                }
            }
        }
    }

    private List<Mistake> getMistakeList(int teacherId, Text text) {
        if (teacherId == 8) {
            return text.getTeacher8Corrections();
        }
        return text.getTeacher9Corrections();
    }

    private Text findByNodeId(int docNodeId) {
        for (Text text : texts) {
            if (text.getNid() == docNodeId) {
                return text;
            }
        }
        return null;
    }

    private Text createTextNode(Node textNode) {
        Text text = new Text();
        for (int i = 0; i < textNode.getChildNodes().getLength(); i++) {
            Node docChild = textNode.getChildNodes().item(i);
            if (isNodeName(docChild, "TITLE")) {
                text.getParagraphs().add(getFirstTextChild(docChild));
            } else if (isNodeName(docChild, "P")) {
                text.getParagraphs().add(getFirstTextChild(docChild));
            }
        }
        return text;
    }

    private String getFirstTextChild(Node node) {
        if (node.getChildNodes().getLength() == 0) {
           return null;
        }
        return node.getChildNodes().item(0).getTextContent();
    }

    private boolean isNodeName(Node docChild, String name) {
        return Node.TEXT_NODE != docChild.getNodeType() && name.equalsIgnoreCase(docChild.getNodeName());
    }

    private Mistake createMistakeFromMode(Node item) {
        Mistake mistake = new Mistake();
        mistake.setEndOff(attrToInt(item, "end_off"));
        mistake.setStartOff(attrToInt(item, "start_off"));
        mistake.setStartPar(attrToInt(item, "start_par"));
        mistake.setStartPar(attrToInt(item, "end_par"));
        for (int i = 0; i < item.getChildNodes().getLength(); i++) {
            Node docChild = item.getChildNodes().item(i);
            if (isNodeName(docChild, "TYPE")) {
                mistake.setType(getFirstTextChild(docChild));
            } else if (isNodeName(docChild, "CORRECTION")) {
                mistake.setCorrection(getFirstTextChild(docChild));
            }
        }
        return mistake;
    }

    private Integer attrToInt(Node item, String attrName) {
        return Integer.valueOf(item.getAttributes().getNamedItem(attrName).getNodeValue());
    }

    private DocumentBuilder getDocumentBuilder() throws ParserConfigurationException {
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        return dbFactory.newDocumentBuilder();
    }
}
