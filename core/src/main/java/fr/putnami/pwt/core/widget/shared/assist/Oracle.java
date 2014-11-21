package fr.putnami.pwt.core.widget.shared.assist;


import java.io.Serializable;
import java.util.Collection;

public interface Oracle<T> {

	interface Callback<T> {
		void onSuggestionsReady(Request request, Response<T> response);
	}

	interface Suggestion<T> {
		String getDisplayString();

		String getReplacementString();

		T getValue();
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
