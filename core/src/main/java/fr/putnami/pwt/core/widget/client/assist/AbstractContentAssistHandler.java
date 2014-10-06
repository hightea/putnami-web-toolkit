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

import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.SuggestOracle;
import com.google.gwt.user.client.ui.SuggestOracle.Suggestion;

public abstract class AbstractContentAssistHandler implements ContentAssistHandler {

	private static final int DEFAULT_LIMIT = 20;

	private SuggestOracle oracle;
	private int limit;

	public AbstractContentAssistHandler(SuggestOracle oracle) {
		this(oracle, AbstractContentAssistHandler.DEFAULT_LIMIT);
	}

	public AbstractContentAssistHandler(SuggestOracle oracle, int limit) {
		this.oracle = oracle;
		this.limit = limit;
	}

	@Override
	public SuggestOracle getOracle() {
		return this.oracle;
	}

	@Override
	public int getLimit() {
		return this.limit;
	}

	@Override
	public void setLimit(int limit) {
		this.limit = limit;
	}

	@Override
	public ContentAssistHandler copy() {
		return this;
	}

	@Override
	public abstract String getQueryText(IsWidget textInput);

	@Override
	public abstract void handleSuggestionSelected(IsWidget textInput, Suggestion suggestion);

}
