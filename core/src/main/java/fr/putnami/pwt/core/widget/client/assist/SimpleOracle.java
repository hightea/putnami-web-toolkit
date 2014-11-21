package fr.putnami.pwt.core.widget.client.assist;

import com.google.common.collect.Lists;
import com.google.common.primitives.Longs;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import fr.putnami.pwt.core.widget.shared.assist.SimpleSuggestion;

public class SimpleOracle<T> extends AbstractOracle<T> {



	private final List<T> allSuggestions = Lists.newArrayList();

	private char[] whitespaceChars;

	public SimpleOracle(char... whitespaceChars) {
		this.whitespaceChars = new char[whitespaceChars.length];
		for (int i = 0; i < whitespaceChars.length; i++) {
			this.whitespaceChars[i] = whitespaceChars[i];
    }
  }

	@Override
	public char[] getWhitespaceChars() {
		return whitespaceChars;
	}

	@Override
	public void setWhitespaceChars(char[] whitespaceChars) {
		this.whitespaceChars = new char[whitespaceChars.length];
		for (int i = 0; i < whitespaceChars.length; i++) {
			this.whitespaceChars[i] = whitespaceChars[i];
		}
	}

	public void clear() {
		allSuggestions.clear();
	}

	public final void addAll(Collection<T> collection) {
		for (T suggestion : collection) {
			add(suggestion);
		}
	}

	public void add(T suggestion) {
		allSuggestions.add(suggestion);
  }

  @Override
	public void doRequest(Request request, Callback<T> callback) {

		List<SimpleSuggestion<T>> suggestions = Lists.newArrayList();

		String query = request.getQuery();
		MultiWordMatcher<T> matcher = new MultiWordMatcher<T>(getRenderer(), getWhitespaceChars());
		for (T value : allSuggestions) {
			int relevance = matcher.match(value, query);
			if (relevance > 0) {
				addToSuggestion(query, suggestions, value, relevance);
			}
		}

		Collections.sort(suggestions, new Comparator<SimpleSuggestion<T>>() {
			@Override
			public int compare(SimpleSuggestion<T> o1, SimpleSuggestion<T> o2) {
				return Longs.compare(o2.getRelevance(), o1.getRelevance());
			}
		});
		Response<T> response = new Response<T>(suggestions);

		int limit = request.getLimit();
		if (limit > 0 && suggestions.size() > limit) {
			response.setMoreSuggestionsCount(suggestions.size() - limit);
			response.setSuggestions(suggestions.subList(0, limit));
		}

		callback.onSuggestionsReady(request, response);
  }
}
