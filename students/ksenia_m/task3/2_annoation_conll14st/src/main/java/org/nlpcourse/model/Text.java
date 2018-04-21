package org.nlpcourse.model;

import java.util.ArrayList;
import java.util.List;

public class Text {
    private int nid;
    private List<String> paragraphs = new ArrayList<>();
    private List<Mistake> teacher8Corrections = new ArrayList<>();
    private List<Mistake> teacher9Corrections = new ArrayList<>();

    public int getNid() {
        return nid;
    }

    public void setNid(int nid) {
        this.nid = nid;
    }

    public List<String> getParagraphs() {
        return paragraphs;
    }

    public void setParagraphs(List<String> paragraphs) {
        this.paragraphs = paragraphs;
    }

    public List<Mistake> getTeacher8Corrections() {
        return teacher8Corrections;
    }

    public void setTeacher8Corrections(List<Mistake> teacher8Corrections) {
        this.teacher8Corrections = teacher8Corrections;
    }

    public List<Mistake> getTeacher9Corrections() {
        return teacher9Corrections;
    }

    public void setTeacher9Corrections(List<Mistake> teacher9Corrections) {
        this.teacher9Corrections = teacher9Corrections;
    }
}
