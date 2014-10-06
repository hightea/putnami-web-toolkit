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
package fr.putnami.pwt.core.widget.client.base;

import com.google.common.base.Objects;
import com.google.gwt.dom.client.InputElement;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.dom.client.HasChangeHandlers;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.text.shared.Parser;
import com.google.gwt.text.shared.Renderer;
import com.google.gwt.user.client.ui.HasEnabled;
import com.google.gwt.user.client.ui.TextBoxBase;
import com.google.web.bindery.event.shared.HandlerRegistration;

import java.text.ParseException;

import fr.putnami.pwt.core.editor.client.event.DirtyEvent.Handler;
import fr.putnami.pwt.core.editor.client.util.ValidationUtils;
import fr.putnami.pwt.core.editor.client.validator.SizeValidator;
import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.model.client.base.HasPlaceholder;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.event.ChangeEvent;
import fr.putnami.pwt.core.widget.client.helper.ToStringRenderer;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractInputBox<T extends TextBoxBase, I> extends AbstractInput<I> implements
HasPlaceholder,
HasEnabled,
HasChangeHandlers
{

	public enum Size implements CssStyle {

		DEFAULT(null),
		LARGE("input-lg"),
		SMALL("input-sm");

		private final String style;

		private Size(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}

	}

	private static final String ERROR_PARSING = "inputParsing";

	private final T input;

	private HandlerRegistration valueChangeRegistration;

	private String inputType;

	private Parser<I> parser;
	private Renderer<I> renderer = (Renderer<I>) ToStringRenderer.get();
	private String placeholder;

	private Size size;

	public AbstractInputBox(T input) {
		super(input);
		this.input = input;
	}

	protected AbstractInputBox(T input, AbstractInputBox<T, I> source) {
		super(input, source);
		this.input = input;
		setPlaceholder(source.placeholder);
		this.parser = source.parser;
		this.renderer = source.renderer;

		setSize(source.size);
		endConstruct();
	}

	public Parser<I> getParser() {
		return parser;
	}

	public void setParser(Parser<I> parser) {
		this.parser = parser;
	}

	public Renderer<I> getRenderer() {
		return renderer;
	}

	public void setRenderer(Renderer<I> renderer) {
		this.renderer = renderer;
	}

	public Size getSize() {
		return size;
	}

	public void setSize(Size size) {
		this.size = size;
		StyleUtils.addStyle(this, size);
	}

	public T getInput() {
		return input;
	}

	public String getType() {
		return inputType;
	}

	public void setInputType(String inputType) {
		this.inputType = inputType;
		getElement().setAttribute("type", inputType);
	}

	@Override
	public String getPlaceholder() {
		return placeholder;
	}

	@Override
	public void setPlaceholder(String placeholder) {
		this.placeholder = placeholder;
		if (placeholder == null) {
			getElement().removeAttribute(HasPlaceholder.PLACEHOLDER_ATTRIBUTE);
		}
		else {
			getElement().setAttribute(HasPlaceholder.PLACEHOLDER_ATTRIBUTE, placeholder);
		}
	}

	@Override
	public boolean isDirty() {
		return !Objects.equal(this.getValue(), input.getValue());
	}

	@Override
	public HandlerRegistration addDirtyHandler(Handler handler) {
		if (valueChangeRegistration == null) {
			valueChangeRegistration = input.addValueChangeHandler(new ChangeEvent<String>(this));
		}
		return super.addDirtyHandler(handler);
	}

	@Override
	public I flush() {
		clearErrors();
		String strValue = getInput().getValue();
		I value = getValue();
		try {
			value = parser.parse(strValue);
			validate(value);
			if (!hasErrors()) {
				setValue(value);
			}
		}
		catch (ParseException e) {
			addError(ValidationUtils.createError(this, ERROR_PARSING, getValue(), strValue));
		}
		return getValue();
	}

	@Override
	public void edit(I value) {
		setValue(value);
		String rendered = renderer.render(value);
		input.setValue(rendered);
	}

	@Override
	public void addValidator(Validator<I> validator) {
		super.addValidator(validator);
		if (validator instanceof SizeValidator) {
			SizeValidator<String> sizeValidator = (SizeValidator<String>) validator;
			if (sizeValidator.getMax() > 0) {
				setMaxLength(sizeValidator.getMax());
			}
		}
	}

	public void setMaxLength(int maxLength) {
		InputElement.as(input.getElement()).setMaxLength(maxLength);
	}

	public int getMaxLength() {
		return InputElement.as(input.getElement()).getMaxLength();
	}

	@Override
	public boolean isEnabled() {
		return input.isEnabled();
	}

	@Override
	public void setEnabled(boolean enabled) {
		input.setEnabled(enabled);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addChangeHandler(ChangeHandler handler) {
		return input.addChangeHandler(handler);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addValueChangeHandler(ValueChangeHandler<I> handler) {
		return input.addHandler(handler, ValueChangeEvent.getType());
	}

}
