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

import com.google.common.base.Preconditions;
import com.google.gwt.regexp.shared.RegExp;

import fr.putnami.pwt.plugin.code.client.token.TokenContent;

public class RegExpWordMatcher extends AbstractWordMatcher {

	private static final String START_INPUT_PATTERN = "^";
	private static final String END_INPUT_PATTERN = "$";

	private final RegExp regexp;

	public RegExpWordMatcher(TokenContent tokenContent, String regExpPattern) {
		super(tokenContent);
		Preconditions.checkArgument(regExpPattern != null, "The RegExp pattern can not be null");
		String fullWordPattern = completePattern(regExpPattern);
		regexp = RegExp.compile(fullWordPattern);
	}

	@Override
	public boolean apply(String input) {
		return regexp.test(input);
	}

	protected String completePattern(String inPattern) {
		StringBuilder result = new StringBuilder(inPattern.length() + 6);
		if (!inPattern.startsWith(START_INPUT_PATTERN)) {
			result.append(START_INPUT_PATTERN);
		}
		result.append(inPattern);
		if (!inPattern.endsWith(END_INPUT_PATTERN)) {
			result.append(END_INPUT_PATTERN);
		}
		return result.toString();
	}

}
