package fr.putnami.pwt.core.widget.shared.assist;

import com.google.common.base.Objects;

import java.io.Serializable;

import fr.putnami.pwt.core.widget.client.assist.SimpleOracle;

public class SimpleSuggestion<T> implements Oracle.Suggestion<T>, Serializable {

	private T value;
	private String replacementString;
  private String displayString;
	private int relevance;

	public SimpleSuggestion() {
	}

	public SimpleSuggestion(T value, String replacementString, String displayString, int relevance) {
		this.value = value;
		this.replacementString = replacementString;
    this.displayString = displayString;
		this.relevance = relevance;
  }

	@Override
	public T getValue() {
		return value;
	}

	public void setValue(T value) {
		this.value = value;
	}

	@Override
	public String getReplacementString() {
		return replacementString;
	}

	public void setReplacementString(String replacementString) {
		this.replacementString = replacementString;
	}

	@Override
	public String getDisplayString() {
		return displayString;
	}

	public void setDisplayString(String displayString) {
		this.displayString = displayString;
	}

	public int getRelevance() {
		return relevance;
	}

	public void setRelevance(int relevance) {
		this.relevance = relevance;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(value);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof SimpleOracle) {
			return Objects.equal(value, value);
		}
		return false;
	}

	@Override
	public String toString() {
		return displayString;
	}

}
