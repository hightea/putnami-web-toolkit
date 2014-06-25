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

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.LIElement;
import com.google.gwt.dom.client.UListElement;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.EditorError;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractWidget;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class ErrorGroup extends AbstractWidget implements EditorError {
	public enum Color implements CssStyle {

		PRIMARY("bg-primary"),
		SUCCESS("bg-success"),
		INFO("bg-info"),
		WARNING("bg-warning"),
		DANGER("bg-danger");

		private final String style;

		private Color(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}

	}

	private String message;

	private Color color = Color.DANGER;

	private boolean hasError;

	public ErrorGroup() {
		super(UListElement.TAG);
		setColor(color);
	}

	public ErrorGroup(ErrorGroup source) {
		super(source);
		setColor(source.color);
		setMessage(source.message);
	}

	@Override
	public IsWidget cloneWidget() {
		return new ErrorGroup(this);
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		StyleUtils.addStyle(this, color);
		this.color = color;
	}

	public void setMessage(String message) {
		this.message = message;
		this.getElement().setInnerText(message);
	}

	public String getMessage() {
		return this.message;
	}

	@Override
	public void clearErrors() {
		getElement().removeAllChildren();
	}

	@Override
	public void displayErrors(Iterable<Error> errors) {
		clearErrors();
		for (Error error : errors) {
			LIElement errorElement = Document.get().createLIElement();
			errorElement.setInnerText(error.getMessageKey());
			this.getElement().appendChild(errorElement);
		}
	}

	public boolean hasError() {
		return getElement().getChildCount() > 0;
	}
}
