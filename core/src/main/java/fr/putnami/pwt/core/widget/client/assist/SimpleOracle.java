package fr.putnami.pwt.core.widget.client.assist;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.primitives.Longs;
import com.google.gwt.core.shared.impl.StringCase;
import com.google.gwt.text.shared.Renderer;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import fr.putnami.pwt.core.widget.client.helper.ToStringRenderer;
import fr.putnami.pwt.core.widget.shared.assist.SimpleSuggestion;

public class SimpleOracle<T> extends AbstractOracle<T> {

  private static final char WHITESPACE_CHAR = ' ';
	private static final String WHITESPACE_STRING = " ";
	private static final String NORMALIZE_TO_SINGLE_WHITE_SPACE = "\\s+";


	private final List<T> allSuggestions = Lists.newArrayList();

	private char[] whitespaceChars;

	private Renderer<T> renderer = ToStringRenderer.<T> get();

  public SimpleOracle() {
		this(WHITESPACE_CHAR);
  }

	public SimpleOracle(char... whitespaceChars) {
		this.whitespaceChars = new char[whitespaceChars.length];
		for (int i = 0; i < whitespaceChars.length; i++) {
			this.whitespaceChars[i] = whitespaceChars[i];
    }
  }

	public char[] getWhitespaceChars() {
		return whitespaceChars;
	}

	public void setWhitespaceChars(char[] whitespaceChars) {
		this.whitespaceChars = new char[whitespaceChars.length];
		for (int i = 0; i < whitespaceChars.length; i++) {
			this.whitespaceChars[i] = whitespaceChars[i];
		}
	}

	public Renderer<T> getRenderer() {
		return renderer;
	}

	public void setRenderer(Renderer<T> renderer) {
		this.renderer = renderer;
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
	public void request(Request request, Callback<T> callback) {
		String query = normalizeSearch(request.getQuery());

		List<SimpleSuggestion<T>> suggestions = Lists.newArrayList();

		Splitter spitter = Splitter.on(WHITESPACE_CHAR).trimResults().omitEmptyStrings();
		for (T value : allSuggestions) {
			String stringSuggested = renderer.render(value);
			spitter.split(stringSuggested);
			if (stringSuggested.equals(query)) {
				addToSuggestion(query, suggestions, value, 10);
			}
			else if (stringSuggested.startsWith(query)) {
				addToSuggestion(query, suggestions, value, 9);
			}
			else{
				for (String word : spitter.split(stringSuggested)) {
					if (word.equals(query)) {
						addToSuggestion(query, suggestions, value, 5);
					}
					else if (word.startsWith(query)) {
						addToSuggestion(query, suggestions, value, 4);
					}
					else if (word.contains(query)) {
						addToSuggestion(query, suggestions, value, 3);
					}
				}
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

	private void addToSuggestion(String query, Collection<SimpleSuggestion<T>> result, T suggestionValue, int relevance) {
		String replacement = renderer.render(suggestionValue);
			String display = replacement.replaceFirst(query, "<strong>" + query + "</strong>");

		SimpleSuggestion<T> suggestion = new SimpleSuggestion<T>(suggestionValue, replacement, display, relevance);
		if (!result.contains(suggestion)) {
			result.add(suggestion);
		}
	}

	private String normalizeSearch(String search) {
		String result = normalizeSuggestion(search);
		result = search.replaceAll(NORMALIZE_TO_SINGLE_WHITE_SPACE, WHITESPACE_STRING);

		return result.trim();
	}

	private String normalizeSuggestion(String formattedSuggestion) {
		String result = StringCase.toLower(formattedSuggestion);
    if (whitespaceChars != null) {
      for (char ignore : whitespaceChars) {
				result = formattedSuggestion.replace(ignore, WHITESPACE_CHAR);
      }
    }
		return result;
  }
}
