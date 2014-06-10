/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client;

import java.util.Collection;

import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.MultiWordSuggestOracle;
import com.google.gwt.user.client.ui.SuggestOracle.Suggestion;
import com.google.gwt.user.client.ui.TextBox;

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

		setParser(StringParser.get());
		setRenderer(StringRenderer.get());

		oracle = new MultiWordSuggestOracle();
		init();
	}

	protected InputSuggest(InputSuggest source) {
		super(new TextBox(), source);
		oracle = source.oracle;
		init();
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputSuggest(this);
	}

	private void init() {
		assistHandler = new TextBoxContentAssistHandler(oracle);
		assistAspect = new ContentAssistAspect(assistHandler);
		assistAspect.setInput(getInput());

		compositeFocus = CompositeFocusHelper.createFocusHelper(this, getInput());
		compositeFocus.addFocusPartner(assistAspect.getSuggestionWidget().getElement());
	}

	public void setSuggestions(Collection<String> suggestions) {
		oracle.clear();
		oracle.setDefaultSuggestionsFromText(suggestions);
		oracle.addAll(suggestions);
	}

	public void setSuggestionsLimit(int limit) {
		assistHandler.setLimit(limit);
	}

	@Override
	public HandlerRegistration addBlurHandler(BlurHandler handler) {
		return compositeFocus.addBlurHandler(handler);
	}

	@Override
	public HandlerRegistration addFocusHandler(FocusHandler handler) {
		return compositeFocus.addFocusHandler(handler);
	}

	class TextBoxContentAssistHandler extends AbstractContentAssistHandler {

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
