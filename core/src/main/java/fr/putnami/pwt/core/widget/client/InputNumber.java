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

import java.math.BigDecimal;
import java.math.BigInteger;

import com.google.gwt.i18n.client.LocaleInfo;
import com.google.gwt.i18n.client.constants.NumberConstants;
import com.google.gwt.text.client.DoubleParser;
import com.google.gwt.text.client.DoubleRenderer;
import com.google.gwt.text.client.IntegerParser;
import com.google.gwt.text.client.IntegerRenderer;
import com.google.gwt.text.client.LongParser;
import com.google.gwt.text.client.LongRenderer;
import com.google.gwt.text.shared.Parser;
import com.google.gwt.text.shared.Renderer;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.TextBox;

import fr.putnami.pwt.core.editor.client.validator.MinValidator;
import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.widget.client.base.AbstractInputBox;
import fr.putnami.pwt.core.widget.client.helper.BigDecimalParser;
import fr.putnami.pwt.core.widget.client.helper.BigDecimalRenderer;
import fr.putnami.pwt.core.widget.client.helper.BigIntegerParser;
import fr.putnami.pwt.core.widget.client.helper.BigIntegerRenderer;
import fr.putnami.pwt.core.widget.client.helper.FloatParser;
import fr.putnami.pwt.core.widget.client.helper.FloatRenderer;
import fr.putnami.pwt.core.widget.client.mask.IntegerTokenHelper;
import fr.putnami.pwt.core.widget.client.mask.MaskValueBoxHelper;
import fr.putnami.pwt.core.widget.client.mask.StaticStringTokenHelper;

public class InputNumber<N extends Number> extends AbstractInputBox<TextBox, N> {

	public static enum NumberType {
		FLOAT(Float.class, FloatRenderer.get(), FloatParser.get()),
		DOUBLE(Double.class, DoubleRenderer.instance(), DoubleParser.instance()),
		BIG_DECIMAL(BigDecimal.class, BigDecimalRenderer.get(), BigDecimalParser.get()),
		INTEGER(Integer.class, IntegerRenderer.instance(), IntegerParser.instance()),
		LONG(Long.class, LongRenderer.instance(), LongParser.instance()),
		BIG_INTEGER(BigInteger.class, BigIntegerRenderer.get(), BigIntegerParser.get());

		private final Class<? extends Number> numberType;
		private final Renderer<? extends Number> renderer;
		private final Parser<? extends Number> parser;

		private NumberType(Class<? extends Number> numberType, Renderer<? extends Number> renderer, Parser<? extends Number> parser) {
			this.numberType = numberType;
			this.renderer = renderer;
			this.parser = parser;
		}

	}

	protected static final NumberConstants NUMBER_CONSTANTS = LocaleInfo.getCurrentLocale().getNumberConstants();

	protected final MaskValueBoxHelper maskHelper;

	private NumberType type;
	private boolean signed = true;

	@UiConstructor
	public InputNumber(NumberType type) {
		super(new TextBox());
		this.type = type;
		maskHelper = new MaskValueBoxHelper(getInput());
		reset();
	}

	protected InputNumber(InputNumber<N> source) {
		super(new TextBox(), source);
		type = source.type;
		signed = source.signed;
		maskHelper = new MaskValueBoxHelper(getInput());
		reset();
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputNumber<N>(this);
	}

	public NumberType getNumberType() {
		return type;
	}

	public void setNumberType(NumberType numberType) {
		this.type = numberType;
		reset();
	}

	public boolean isSigned() {
		return signed;
	}

	public void setSigned(boolean signed) {
		this.signed = signed;
		reset();
	}

	@Override
	public void addValidator(Validator<N> validator) {
		super.addValidator(validator);
		if (signed && validator instanceof MinValidator) {
			MinValidator minValidator = (MinValidator) validator;
			if (minValidator.getMin() >= 0) {
				this.signed = false;
				reset();
			}
		}
	}

	protected void reset() {
		if (type == null) {
			type = type.LONG;
		}

		setRenderer((Renderer<N>) type.renderer);
		setParser((Parser<N>) type.parser);

		maskHelper.reset();
		if (signed) {
			maskHelper.addTokenHelper(new StaticStringTokenHelper(NUMBER_CONSTANTS.minusSign(), true));
		}
		maskHelper.addTokenHelper(new IntegerTokenHelper(0, Integer.MIN_VALUE, Integer.MAX_VALUE, -1, "0"));

		switch (type) {
		case FLOAT:
		case DOUBLE:
		case BIG_DECIMAL:
			maskHelper.addTokenHelper(new StaticStringTokenHelper(NUMBER_CONSTANTS.decimalSeparator(), true, ',', '.'));
			maskHelper.addTokenHelper(new IntegerTokenHelper(0, Integer.MIN_VALUE, Integer.MAX_VALUE, -1, "0"));
			break;
		default:
			break;
		}
	}

}
