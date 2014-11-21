package fr.putnami.pwt.core.widget.client.assist;

import com.google.common.base.Splitter;
import com.google.gwt.core.shared.impl.StringCase;
import com.google.gwt.text.shared.Renderer;

public class MultiWordMatcher<T> {

	private static final char WHITESPACE_CHAR = ' ';

	private Splitter spitter = Splitter.on(WHITESPACE_CHAR).trimResults().omitEmptyStrings();

	private Renderer<T> renderer;
	private char[] whitespaceChars;

	public MultiWordMatcher(Renderer<T> renderer, char[] whitespaceChars) {
		this.renderer = renderer;
		this.whitespaceChars = whitespaceChars;
	}

	public int match(T value, String query) {
		String normalizedSuggestion = normalize(renderer.render(value));
		String normalizedQuery = normalize(query);

		spitter.split(normalizedSuggestion);
		if (normalizedSuggestion.equals(normalizedQuery)) {
			return 10;
		}
		else if (normalizedSuggestion.startsWith(normalizedQuery)) {
			return 9;
		}
		else{
			for (String word : spitter.split(normalizedSuggestion)) {
				if (word.equals(normalizedQuery)) {
					return 5;
				}
				else if (word.startsWith(normalizedQuery)) {
					return 4;
				}
				else if (word.contains(normalizedQuery)) {
					return 3;
				}
			}
		}
		return 0;
	}

	private String normalize(String formattedSuggestion) {
		String result = StringCase.toLower(formattedSuggestion);
		if (whitespaceChars != null) {
			for (char ignore : whitespaceChars) {
				result = formattedSuggestion.replace(ignore, WHITESPACE_CHAR);
			}
		}
		return result;
	}

}
