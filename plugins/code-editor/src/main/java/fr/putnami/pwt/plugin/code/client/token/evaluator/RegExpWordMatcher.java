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
		String fullWordPattern = this.completePattern(regExpPattern);
		this.regexp = RegExp.compile(fullWordPattern);
	}

	@Override
	public boolean apply(String input) {
		return this.regexp.test(input);
	}

	protected String completePattern(String inPattern) {
		StringBuilder result = new StringBuilder(inPattern.length() + 6);
		if (!inPattern.startsWith(RegExpWordMatcher.START_INPUT_PATTERN)) {
			result.append(RegExpWordMatcher.START_INPUT_PATTERN);
		}
		result.append(inPattern);
		if (!inPattern.endsWith(RegExpWordMatcher.END_INPUT_PATTERN)) {
			result.append(RegExpWordMatcher.END_INPUT_PATTERN);
		}
		return result.toString();
	}

}
