package fr.putnami.pwt.core.widget.shared.assist;

import com.google.common.base.Objects;

import java.io.Serializable;

import fr.putnami.pwt.core.widget.client.assist.SimpleOracle;

public class SimpleSuggestion<T> implements Oracle.Suggestion<T>, Serializable {

	private T value;
	private int relevance;

	public SimpleSuggestion() {
	}

	public SimpleSuggestion(T value, int relevance) {
		this.value = value;
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

}
