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
import java.util.Collections;
import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.EditorCollection;
import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.EditorValue;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.Visitor;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.EditorModel;
import fr.putnami.pwt.core.model.client.base.HasDriver;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.model.ModelCollection;
import fr.putnami.pwt.core.model.client.visitor.ReadonlyVisitor;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumn;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class TableEditorBody<T> extends TableBody<T> implements
		HasDriver<Collection<T>, ModelDriver<Collection<T>>>,
		EditorOutput<Collection<T>>,
		EditorInput<Collection<T>>,
		EditorCollection<T>,
		EditorModel<T> {

	private final List<AbstractTableColumn<?>> columns = Lists.newArrayList();

	private MessageHelper messageHelper;
	private Model<T> model;
	private ModelDriver<Collection<T>> driver;

	@UiConstructor
	public TableEditorBody(String bodyId) {
		super(bodyId);
	}

	protected TableEditorBody(TableEditorBody<T> source) {
		super(source);
		for (AbstractTableColumn<?> col : source.columns) {
			addColumn(WidgetUtils.cloneWidget(col));
		}
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableEditorBody<T>(this);
	}

	@Override
	public Model<T> getModel() {
		return this.model;
	}

	@Override
	public void setMessageHelper(MessageHelper messageHelper) {
		this.messageHelper = messageHelper;
	}

	@Override
	public void initialize(Model<T> model, Visitor... visitors) {
		assert this.model == null : "model can not be set twice.";
		this.model = model;
		this.driver = new ModelDriver<Collection<T>>((ModelCollection<T>) model);
		this.driver.setMessageHelper(messageHelper);
		this.driver.initialize(this, visitors);
		this.driver.accept(new ReadonlyVisitor(this, getReadonly(), true));
	}

	@Override
	public ModelDriver<Collection<T>> getDriver() {
		return driver;
	}

	@Override
	public void setDriver(ModelDriver<Collection<T>> driver) {
		// Nothing to do, initialised method must be call in order to collect contexts
	}

	@Override
	public void setReadonly(Boolean readonly) {
		super.setReadonly(readonly);
		if (this.driver != null) {
			this.driver.accept(new ReadonlyVisitor(this, readonly, true));
		}
	}

	@Override
	public void add(IsWidget w) {
		super.add(w);
		if (w instanceof AbstractTableColumn) {
			this.addColumn((AbstractTableColumn) w);
		}
	}

	private <A> void addColumn(AbstractTableColumn<A> column) {
		this.columns.add(column);
	}

	@Override
	public Collection<T> flush() {
		return this.driver.flush();
	}

	@Override
	public void edit(Collection<T> value) {
		for (TableRow<T> row : getRowList()) {
			row.setVisible(false);
		}
		this.driver.edit(value);
	}

	@Override
	public <A extends EditorValue<T>> A getEditorForTraversal(int index) {
		TableRow<T> row = null;
		if (getRowList().size() > index) {
			row = getRowList().get(index);
		}
		if (row == null) {
			row = new TableRow<T>();
			row.setIndex(index);
			for (AbstractTableColumn<?> column : this.columns) {
				row.append(column.createBodyCell());
			}
			row.setReadonly(getReadonly());
			row.setMessageHelper(messageHelper);
			row.initialize((Model<T>) this.driver.getModel().getLeafModel());

			addRow(row);
		}
		row.setVisible(true);
		return (A) row;
	}

	public void switchRows(TableRow<T> first, TableRow<T> second) {
		List<TableRow<T>> rows = getRowList();
		int firstIndex = rows.indexOf(first);
		int secondIndex = rows.indexOf(second);
		if (firstIndex == secondIndex) {
			return;
		}
		else if (firstIndex < secondIndex) {
			rows.remove(firstIndex);
			rows.add(firstIndex, second);
			rows.remove(secondIndex);
			rows.add(secondIndex, first);
			insert(second, firstIndex, true);
		}
		else {
			rows.remove(secondIndex);
			rows.add(secondIndex, first);
			rows.remove(firstIndex);
			rows.add(firstIndex, second);
			insert(first, secondIndex, true);
		}
	}

	@Override
	public Collection<T> getValue() {
		return driver == null ? null : driver.getValue();
	}

	@Override
	public boolean hasErrors() {
		return driver == null ? false : driver.hasErrors();
	}

	@Override
	public Iterable<Error> getErrors() {
		return driver == null ? Collections.EMPTY_LIST : driver.getErrors();
	}

	@Override
	public void addValidator(Validator<Collection<T>> validator) {
	}

}
