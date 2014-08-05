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

import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorLeaf;
import fr.putnami.pwt.core.editor.client.EditorOutput;
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
import fr.putnami.pwt.core.widget.client.helper.PaginationHelper;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class TableEditor<T> extends Table<T> implements
HasDriver<Collection<T>, ModelDriver<Collection<T>>>,
EditorLeaf,
EditorOutput<Collection<T>>,
EditorInput<Collection<T>>,
EditorModel<T> {

	private MessageHelper messageHelper;
	private Model<T> model;
	private ModelDriver<Collection<T>> driver;

	private PaginationHelper<T> pagination;

	public TableEditor() {
		super();
	}

	protected TableEditor(TableEditor<T> source) {
		super(source);
		setPaginationHelper(WidgetUtils.cloneWidget(source.pagination));
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableEditor<T>(this);
	}

	@Override
	protected TableBody<T> createBody(String bodyId) {
		TableBody<T> body = new TableEditorBody<T>(bodyId);
		body.setReadonly(getReadonly());
		return body;
	}

	@Override
	public Model<T> getModel() {
		return this.model;
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
		super.add(w); // Table.add(IsWidget);
		if (w instanceof Pagination) {
			this.setPagination((Pagination) w);
		}
		if (w instanceof AbstractTableColumn) {
			this.addColumn((AbstractTableColumn) w);
		}
	}

	@Override
	public void setMessageHelper(MessageHelper messageHelper) {
		this.messageHelper = messageHelper;
	}

	@Override
	public void initialize(Model<T> model, Visitor... visitors) {
		assert this.model == null : "model can not be set twice.";
		this.model = model;
		this.driver = new ModelDriver<Collection<T>>(new ModelCollection<T>(List.class, model));
		this.driver.setMessageHelper(messageHelper);
		this.driver.initialize(this, visitors);
		this.driver.accept(new ReadonlyVisitor(this, getReadonly(), true));
		if (pagination != null) {
			driver.registerVisitor(pagination);
		}
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
	public Collection<T> flush() {
		return this.driver.flush();
	}

	@Override
	public void edit(Collection<T> value) {
		driver.edit(value);
	}

	public <A> boolean removeColumn(AbstractTableColumn<A> column) {
		ensureTableHead().removeColumn(column);
		TableBody<T> body = getDefaultBody();
		if (body instanceof TableEditorBody) {
			((TableEditorBody) body).removeColumn(column);
		}
		return true;
	}

	private void setPagination(Pagination pagination) {
		setPaginationHelper(new PaginationHelper<T>(pagination));
	}

	private void setPaginationHelper(PaginationHelper<T> paginationHelper) {
		this.pagination = paginationHelper;
		append(this.pagination);
		if (driver != null) {
			driver.registerVisitor(this.pagination);
		}
	}

	private <A> void addColumn(AbstractTableColumn<A> column) {
		ensureTableHead().add(column);
		getDefaultBody().add(column);
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
