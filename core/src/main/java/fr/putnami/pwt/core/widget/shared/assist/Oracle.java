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
package fr.putnami.pwt.core.widget.shared.assist;

import java.io.Serializable;
import java.util.Collection;

public interface Oracle<T> {

	interface Callback<T> {
		void onSuggestionsReady(Request request, Response<T> response);
	}

	interface Suggestion<T> {
		T getValue();

		int getRelevance();
	}

	interface Highlighter<T> {
		String highlight(T value, String query);
	}

	class Request implements Serializable {
		private int limit = 20;
		private String query;

		public Request() {
		}

		public Request(String query) {
			setQuery(query);
		}

		public Request(String query, int limit) {
			setQuery(query);
			setLimit(limit);
		}

		public int getLimit() {
			return limit;
		}

		public String getQuery() {
			return query;
		}

		public void setLimit(int limit) {
			this.limit = limit;
		}

		public void setQuery(String query) {
			this.query = query;
		}
	}

	class Response<T> implements Serializable {
		private Collection<? extends Suggestion<T>> suggestions;
		private boolean moreSuggestions = false;
		private int numMoreSuggestions = 0;

		public Response() {
		}

		public Response(Collection<? extends Suggestion<T>> suggestions) {
			setSuggestions(suggestions);
		}

		public int getMoreSuggestionsCount() {
			return this.numMoreSuggestions;
		}

		public Collection<? extends Suggestion<T>> getSuggestions() {
			return this.suggestions;
		}

		public boolean hasMoreSuggestions() {
			return this.moreSuggestions;
		}

		public void setMoreSuggestions(boolean moreSuggestions) {
			this.moreSuggestions = moreSuggestions;
		}

		public void setMoreSuggestionsCount(int count) {
			this.numMoreSuggestions = count;
			this.moreSuggestions = count > 0;
		}

		public void setSuggestions(Collection<? extends Suggestion<T>> suggestions) {
			this.suggestions = suggestions;
		}
	}

	void request(Request request, Callback<T> callback);
}
