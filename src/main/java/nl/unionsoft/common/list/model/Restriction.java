package nl.unionsoft.common.list.model;

public class Restriction {

    public Restriction (Rule rule) {
        this.rule = rule;
    }

    private Rule rule;

    public Rule getRule() {
        return rule;
    }

    public void setRule(Rule rule) {
        this.rule = rule;
    }

    public enum Rule {
        EQ, NE, GT, GE, LT, LE, NOT_NULL, NULL, AND, OR, LIKE;
    }

}
