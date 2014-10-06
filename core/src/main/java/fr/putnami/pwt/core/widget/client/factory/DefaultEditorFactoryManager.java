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
package fr.putnami.pwt.core.widget.client.factory;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import fr.putnami.pwt.core.model.client.factory.EditorFactoryManager;
import fr.putnami.pwt.core.widget.client.InputBoolean;
import fr.putnami.pwt.core.widget.client.InputDate;
import fr.putnami.pwt.core.widget.client.InputList;
import fr.putnami.pwt.core.widget.client.InputNumber;
import fr.putnami.pwt.core.widget.client.InputNumber.NumberType;
import fr.putnami.pwt.core.widget.client.InputSelect;
import fr.putnami.pwt.core.widget.client.InputText;
import fr.putnami.pwt.core.widget.client.OutputBoolean;
import fr.putnami.pwt.core.widget.client.OutputDate;
import fr.putnami.pwt.core.widget.client.OutputEnum;
import fr.putnami.pwt.core.widget.client.OutputList;
import fr.putnami.pwt.core.widget.client.OutputNumber;
import fr.putnami.pwt.core.widget.client.OutputText;

public class DefaultEditorFactoryManager extends EditorFactoryManager {

	public DefaultEditorFactoryManager() {
		// Register labelFactory
		this.setLabelFactory(LabelEditorFactory.INSTANCE);
		// Register tooltipFactory
		this.setTooltipFactory(TooltipEditorFactory.INSTANCE);

		// Register inputFactories
		{
			this.registerInputFactory(String.class, new InputText());

			this.registerInputFactory(BigDecimal.class, new InputNumber<Double>(NumberType.DOUBLE));
			this.registerInputFactory(Double.class, new InputNumber<Double>(NumberType.DOUBLE));
			this.registerInputFactory(double.class, new InputNumber<Double>(NumberType.DOUBLE));
			this.registerInputFactory(Float.class, new InputNumber<Float>(NumberType.FLOAT));
			this.registerInputFactory(float.class, new InputNumber<Float>(NumberType.FLOAT));
			this.registerInputFactory(Integer.class, new InputNumber<Integer>(NumberType.INTEGER));
			this.registerInputFactory(int.class, new InputNumber<Integer>(NumberType.INTEGER));
			this.registerInputFactory(Long.class, new InputNumber<Long>(NumberType.LONG));
			this.registerInputFactory(long.class, new InputNumber<Long>(NumberType.LONG));

			this.registerInputFactory(Date.class, new InputDate());

			this.registerInputFactory(Boolean.class, new InputBoolean());
			this.registerInputFactory(boolean.class, new InputBoolean());

			this.registerInputFactory(Enum.class, new InputSelect());

			this.registerInputFactory(List.class, new InputList());
			this.registerInputFactory(Collection.class, new InputList());
		}
		// Register outputFactories
		{
			this.registerOutputFactory(String.class, new OutputText());

			this.registerOutputFactory(BigDecimal.class, new OutputNumber());
			this.registerOutputFactory(Double.class, new OutputNumber());
			this.registerOutputFactory(double.class, new OutputNumber());
			this.registerOutputFactory(Float.class, new OutputNumber());
			this.registerOutputFactory(float.class, new OutputNumber());
			this.registerOutputFactory(Integer.class, new OutputNumber());
			this.registerOutputFactory(int.class, new OutputNumber());
			this.registerOutputFactory(Long.class, new OutputNumber());
			this.registerOutputFactory(long.class, new OutputNumber());

			this.registerOutputFactory(Date.class, new OutputDate());

			this.registerOutputFactory(Boolean.class, new OutputBoolean());
			this.registerOutputFactory(boolean.class, new OutputBoolean());

			this.registerOutputFactory(Enum.class, new OutputEnum());

			this.registerOutputFactory(List.class, new OutputList());
			this.registerOutputFactory(Collection.class, new OutputList());
		}
	}
}
