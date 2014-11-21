package fr.putnami.pwt.core.widget.client.assist;

import com.google.gwt.text.shared.Renderer;

import java.util.Collection;

import fr.putnami.pwt.core.widget.client.helper.ToStringRenderer;
import fr.putnami.pwt.core.widget.shared.assist.Oracle;
import fr.putnami.pwt.core.widget.shared.assist.SimpleSuggestion;

public abstract class AbstractOracle<T> implements Oracle<T> {

	private int queryLengthToRequest = 0;

	private Renderer<T> renderer = ToStringRenderer.<T> get();

	private char[] whitespaceChars;
	private static final char WHITESPACE_CHAR = ' ';

	public AbstractOracle() {
		this(WHITESPACE_CHAR);
	}

	public AbstractOracle(char... whitespaceChars) {
		this.whitespaceChars = new char[whitespaceChars.length];
		for (int i = 0; i < whitespaceChars.length; i++) {
			this.whitespaceChars[i] = whitespaceChars[i];
		}
	}

	public char[] getWhitespaceChars() {
		return whitespaceChars;
	}

	public void setWhitespaceChars(char[] whitespaceChars) {
		this.whitespaceChars = whitespaceChars;
	}

	public Renderer<T> getRenderer() {
		return renderer;
	}

	public void setRenderer(Renderer<T> renderer) {
		this.renderer = renderer;
	}

	public int getQueryLengthToRequest() {
		return queryLengthToRequest;
	}

	public void setQueryLengthToRequest(int queryLengthToRequest) {
		this.queryLengthToRequest = queryLengthToRequest;
	}

	@Override
	public void request(Oracle.Request request, Oracle.Callback<T> callback) {
		String query = request.getQuery();
		int queryLenght = query == null ? 0 : query.length();
		if (queryLenght >= queryLengthToRequest) {
			doRequest(request, callback);
		}
	}

	protected void addToSuggestion(String query, Collection<SimpleSuggestion<T>> result, T suggestionValue, int relevance) {
		String replacement = renderer.render(suggestionValue);
		String display = replacement.replaceFirst(query, "<strong>" + query + "</strong>");

		SimpleSuggestion<T> suggestion = new SimpleSuggestion<T>(suggestionValue, replacement, display, relevance);
		if (!result.contains(suggestion)) {
			result.add(suggestion);
		}
	}

	protected abstract void doRequest(Oracle.Request request, Oracle.Callback<T> callback);

}
