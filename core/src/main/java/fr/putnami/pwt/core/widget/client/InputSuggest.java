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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.MultiWordSuggestOracle;
import com.google.gwt.user.client.ui.SuggestOracle.Suggestion;
import com.google.gwt.user.client.ui.TextBox;

import java.util.Collection;

import fr.putnami.pwt.core.widget.client.assist.AbstractContentAssistHandler;
import fr.putnami.pwt.core.widget.client.assist.ContentAssistAspect;
import fr.putnami.pwt.core.widget.client.assist.ContentAssistHandler;
import fr.putnami.pwt.core.widget.client.base.AbstractInputBox;
import fr.putnami.pwt.core.widget.client.helper.CompositeFocusHelper;
import fr.putnami.pwt.core.widget.client.helper.StringParser;
import fr.putnami.pwt.core.widget.client.helper.StringRenderer;

public class InputSuggest extends AbstractInputBox<TextBox, String> {

	private MultiWordSuggestOracle oracle;
	private ContentAssistHandler assistHandler;
	private ContentAssistAspect assistAspect;

	private CompositeFocusHelper compositeFocus;

	public InputSuggest() {
		super(new TextBox());

		this.setParser(StringParser.get());
		this.setRenderer(StringRenderer.get());

		this.oracle = new MultiWordSuggestOracle();
		this.init();
	}

	protected InputSuggest(InputSuggest source) {
		super(new TextBox(), source);
		this.oracle = source.oracle;
		this.init();
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputSuggest(this);
	}

	private void init() {
		this.assistHandler = new TextBoxContentAssistHandler(this.oracle);
		this.assistAspect = new ContentAssistAspect(this.assistHandler);
		this.assistAspect.setInput(this.getInput());

		this.compositeFocus = CompositeFocusHelper.createFocusHelper(this, this.getInput());
		this.compositeFocus.addFocusPartner(this.assistAspect.getSuggestionWidget().getElement());
	}

	public void setSuggestions(Collection<String> suggestions) {
		this.oracle.clear();
		this.oracle.setDefaultSuggestionsFromText(suggestions);
		this.oracle.addAll(suggestions);
	}

	public void setSuggestionsLimit(int limit) {
		this.assistHandler.setLimit(limit);
	}

	@Override
	public HandlerRegistration addBlurHandler(BlurHandler handler) {
		return this.compositeFocus.addBlurHandler(handler);
	}

	@Override
	public HandlerRegistration addFocusHandler(FocusHandler handler) {
		return this.compositeFocus.addFocusHandler(handler);
	}

	static class TextBoxContentAssistHandler extends AbstractContentAssistHandler {

		public TextBoxContentAssistHandler(MultiWordSuggestOracle oracle) {
			super(oracle);
		}

		@Override
		public String getQueryText(IsWidget textInput) {
			TextBox input = (TextBox) textInput;
			return input.getText();
		}

		@Override
		public void handleSuggestionSelected(IsWidget textInput, Suggestion suggestion) {
			TextBox input = (TextBox) textInput;
			input.setText(suggestion.getReplacementString());
			input.setCursorPos(suggestion.getReplacementString().length());
		}
	}
}
