/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client.assist;

import com.google.gwt.text.shared.Renderer;

import fr.putnami.pwt.core.widget.client.helper.ToStringRenderer;
import fr.putnami.pwt.core.widget.shared.assist.Oracle;
import fr.putnami.pwt.core.widget.shared.assist.SimpleSuggestion;

public abstract class AbstractOracle<T> implements Oracle<T> {

	private int queryLengthToRequest = 0;

	private Renderer<T> renderer = ToStringRenderer.<T> get();

	private char[] whitespaceChars;
	private static final char WHITESPACE_CHAR = ' ';

	private Matcher<T> matcher;

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

	public Matcher<T> getMatcher() {
		if (matcher == null) {
			matcher = new MultiWordMatcher<T>(getRenderer(), getWhitespaceChars());
		}
		return matcher;
	}

	public void setMatcher(Matcher<T> matcher) {
		this.matcher = matcher;
	}

	@Override
	public void request(Oracle.Request request, Oracle.Callback<T> callback) {
		String query = request.getQuery();
		int queryLenght = query == null ? 0 : query.length();
		if (queryLenght >= queryLengthToRequest) {
			doRequest(request, callback);
		}
	}

	protected <S extends Suggestion<T>> S newSuggestion(String query, T value, int relevance) {
		return (S) new SimpleSuggestion<T>(value, relevance);
	}

	protected abstract void doRequest(Oracle.Request request, Oracle.Callback<T> callback);

}
