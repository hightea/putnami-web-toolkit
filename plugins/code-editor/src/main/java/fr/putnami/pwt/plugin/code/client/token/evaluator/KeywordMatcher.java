/**
 * This file is part of pwt-code-editor.
 *
 * pwt-code-editor is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-code-editor is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-code-editor.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.code.client.token.evaluator;

import java.util.List;

import com.google.common.collect.Lists;

import fr.putnami.pwt.plugin.code.client.token.TokenContent;

public class KeywordMatcher extends AbstractWordMatcher {

	private final List<String> words = Lists.newArrayList();

	public KeywordMatcher(TokenContent tokenContent, List<String> words) {
		super(tokenContent);
		this.words.addAll(words);
	}

	public KeywordMatcher(TokenContent tokenContent, String... words) {
		this(tokenContent, Lists.newArrayList(words));
	}

	@Override
	public boolean apply(String input) {
		return words.contains(input);
	}

}
