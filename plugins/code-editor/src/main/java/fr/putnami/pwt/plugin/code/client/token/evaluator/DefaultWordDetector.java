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

import com.google.common.base.CharMatcher;

public class DefaultWordDetector implements WordDetector {

	public static final WordDetector INSTANCE = new DefaultWordDetector();

	public boolean isWordStart(char c) {
		return CharMatcher.JAVA_LETTER.matches(c);
	}

	public boolean isWordPart(char c) {
		return CharMatcher.JAVA_LETTER.matches(c);
	}
}
